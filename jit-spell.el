;;; jit-spell.el --- Just-in-time spell checking      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: tools, wp
;; URL: https://github.com/astoff/jit-spell
;; Package-Requires: ((emacs "27.1") (compat "29.1"))
;; Version: 0.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `jit-spell-mode', a local minor mode to
;; highlight all misspelled words in a window, just like a word
;; processor or web browser does.
;;
;; This behavior is different from Flyspell, which only checks words
;; as the cursor moves over them.  Moreover, unlike Flyspell,
;; jit-spell communicates with the spell-checking subprocess entirely
;; asynchronously, which can lead to a noticeable performance
;; improvement.
;;
;; To set up jit-spell, add your desired variation of the following to
;; your init file:
;;
;;   (add-hook 'text-mode-hook 'jit-spell-mode)
;;   (add-hook 'prog-mode-hook 'jit-spell-mode)
;;   (with-eval-after-load 'jit-spell
;;     (define-key jit-spell-mode-map (kbd "C-;") 'jit-spell-correct-word))
;;
;; jit-spell relies on the `ispell' library to pick a spell checking
;; program and dictionaries.  Try `M-x customize-group RET ispell RET'
;; to see a listing of all possible settings.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'ispell)
(require 'text-property-search)

(defgroup jit-spell nil
  "Check spelling as you type."
  :prefix "jit-spell-"
  :group 'ispell
  :group 'text)

(defface jit-spell-misspelling
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "red"))
    (t
     :underline t :inherit error))
  "Face added by `jit-spell-mode' to misspelled words.")

(defcustom jit-spell-current-word-delay 3
  "Time, in seconds, to wait before highlighting word at point."
  :type 'number)

(defcustom jit-spell-ignored-faces
  '(message-header-name)
  "Faces jit-spell should ignore."
  :type '(repeat face))

(defcustom jit-spell-tex-ignored-faces
  '(font-latex-math-face
    font-latex-verbatim-face
    font-latex-sedate-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-variable-name-face
    tex-math
    tex-verbatim)
  "Faces jit-spell should ignore in TeX and derived modes.
You can modify this variable buffer locally, say in a mode hook,
but this must be done before activating `jit-spell-mode.'"
  :type '(repeat face))

(defcustom jit-spell-prog-mode-faces
  '(font-lock-comment-face
    font-lock-doc-face
    font-lock-string-face)
  "Faces jit-spell should check in modes derived from `prog-mode'.
You can modify this variable buffer locally, say in a mode hook,
but this must done before activating `jit-spell-mode.'"
  :type '(repeat face))

(defcustom jit-spell-use-apostrophe-hack 'auto
  "Whether to work around Hunspell's issue parsing apostrophes.
In some languages, Hunspell always considers the apostrophe
character (a.k.a. straight quote) part of the word, which leads
to false positives when it is used as a quotation mark."
  :type '(choice (const :tag "Decide automatically" auto)
                 (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom jit-spell-correct-next-key 'auto
  "Key used to jump to the next misspelling in `jit-spell-correct-word'.
This can be a key sequence, nil, or `auto' to use the same key to
which the command `jit-spell-correct-word' is bound."
  :type '(choice (const :tag "Determine automatically" auto)
                 (string :tag "Key")
                 (const :tag "None" nil)))

(defvar jit-spell--ignored-p #'jit-spell--default-ignored-p
  "Predicate satisfied by words to ignore.
It should be a function taking two arguments, the start and end
positions of the word.")

(defvar jit-spell--filter-region #'jit-spell--filter-region
  "Function to extract regions of interest from the buffer.
It receives the start and end positions of a region as argument,
and should return a list of cons cells representing subregions.

This function is called inside a `save-excursion' form and can
move the point with impunity.")

(defvar jit-spell--process-pool nil
  "A collection of ispell processes for `jit-spell-mode'.")

(defvar jit-spell--hidden-overlay nil
  "Place to keep track of a hidden overlay near the cursor.")

(defvar-local jit-spell--recheck-timer nil
  "Timer to debounce recheck requests.")

(defvar-local jit-spell--local-words nil
  "A list of words accepted temporarily in this buffer.")

;;; Overlays

(put 'jit-spell 'evaporate t)
(put 'jit-spell 'face 'jit-spell-misspelling)

(defun jit-spell--make-overlay (start end corrections)
  "Make an overlay to highlight incorrect word between START and END.
Also add the list of CORRECTIONS as a property."
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'jit-spell-corrections corrections)
    (if (not (<= start (point) end))
        (overlay-put ov 'category 'jit-spell)
      (jit-spell--unhide-overlay)
      (setq jit-spell--hidden-overlay
            (cons ov (run-with-timer jit-spell-current-word-delay nil
                                     #'jit-spell--unhide-overlay))))
    ov))

(defun jit-spell--make-overlays (buffer start end misspellings)
  "Create jit-spell overlays in BUFFER between START and END.
MISSPELLINGS is a list with elements consisting of a word, a
character offset from START, and a list of corrections."
  (with-current-buffer buffer
    (with-silent-modifications
      (remove-list-of-text-properties start end '(jit-spell-pending))
      (jit-spell--remove-overlays start end)
      (pcase-dolist (`(,word ,offset ,corrections) misspellings)
        (let* ((wstart (+ start offset -1))
               (wend (+ wstart (length word))))
          (unless (funcall jit-spell--ignored-p wstart wend)
            (jit-spell--make-overlay wstart wend corrections)))))))

(defun jit-spell--overlay-at (pos)
  "Return the jit-spell overlay at POS, if it exists."
  (seq-some (lambda (ov)
              (and (eq (overlay-get ov 'category) 'jit-spell) ov))
            (overlays-at pos)))

(defun jit-spell--search-overlay (pos count)
  "Return the COUNT jit-spell overlay from POS."
  (let* ((limit (if (cl-plusp count) (window-end) (window-start)))
         (searchfn (if (cl-plusp count)
                       #'next-single-char-property-change
                     #'previous-single-char-property-change))
         (i (abs count)))
    (catch 'jit-spell
      (while (/= pos limit)
        (setq pos (funcall searchfn pos 'category nil limit))
        (when-let ((ov (jit-spell--overlay-at pos)))
          (unless (cl-plusp (cl-decf i))
            (throw 'jit-spell ov)))))))

(defun jit-spell--remove-overlays (start end &optional gaps)
  "Remove all `jit-spell' overlays between START and END, skipping GAPS.
GAPS must be an ordered list of conses, with the intervals closer
to END coming first."
  (named-let recur ((i (or (cdar gaps) start))
                    (j end)
                    (gaps gaps))
    (dolist (ov (overlays-in i j))
      (when (or (eq (overlay-get ov 'category) 'jit-spell)
                (eq ov (car jit-spell--hidden-overlay)))
        (delete-overlay ov)))
    (when gaps
      (recur (or (cdadr gaps) start) (caar gaps) (cdr gaps)))))

(defun jit-spell--apply-correction (ov text)
  "Replace region spanned by OV with TEXT."
  (save-excursion
    (goto-char (overlay-start ov))
    (delete-region (point) (overlay-end ov))
    (insert-before-markers text)))

(defun jit-spell--unhide-overlay ()
  "Unhide the overlay stored in `jit-spell--hidden-overlay'."
  (when jit-spell--hidden-overlay
    (overlay-put (car jit-spell--hidden-overlay) 'category 'jit-spell)
    (cancel-timer (cdr jit-spell--hidden-overlay))
    (setq jit-spell--hidden-overlay nil)))

(defun jit-spell--unhide-overlay-maybe ()
  "Unhide `jit-spell--hidden-overlay' if it doesn't overlap with word at point."
  (when-let ((ov (car jit-spell--hidden-overlay)))
    (unless (and (eq (overlay-buffer ov) (current-buffer))
                 (save-excursion
                   (re-search-forward (rx (or blank eol)))
                   (<= (overlay-start ov) (match-beginning 0)))
                 (save-excursion
                   (re-search-backward (rx (or blank bol)))
                   (<= (match-end 0) (overlay-end ov))))
      (jit-spell--unhide-overlay))))

(defun jit-spell--unfontify (&optional start end lax)
  "Remove overlays and forget checking status from START to END (or whole buffer).
Force refontification of the region, unless LAX is non-nil."
  ;; Drop pending requests for this buffer
  (pcase-dolist (`(,_ . ,proc) jit-spell--process-pool)
    (setf (process-get proc 'jit-spell--requests)
          (seq-filter (lambda (r) (not (eq (car r) (current-buffer))))
                      (process-get proc 'jit-spell--requests)))
    (while (eq (car (process-get proc 'jit-spell--current-request))
               (current-buffer))
      (accept-process-output proc)))
  (without-restriction
    (setq start (or start (point-min)))
    (setq end (or end (point-max)))
    (jit-spell--remove-overlays start end)
    (remove-list-of-text-properties start end '(jit-spell-pending))
    (unless lax (jit-lock-refontify start end))))

;;; Subprocess communication

(defun jit-spell--process-parameters ()
  "Return a list of parameters for this buffer's ispell process."
  (list ispell-program-name
        (or ispell-local-dictionary ispell-dictionary)
        (or ispell-local-pdict ispell-personal-dictionary)
        ispell-extra-args))

(defun jit-spell--get-process ()
  "Get an ispell process for the current buffer.
Buffers where `jit-spell--process-parameters' returns the `equal'
results share their ispell process.

The process plist includes the following properties:

`jit-spell--current request': a list of the form
    (BUFFER TICK START END)
  where TICK is used to keep track of the timeliness of the response.
  Note that the process receives a single request at a time.

`jit-spell--requests': a FIFO queue with elements in the same
  form as above."
  (let* ((params (jit-spell--process-parameters))
         (proc (cdr (assoc params jit-spell--process-pool))))
    (if (process-live-p proc)
        proc
      (unless ispell-async-processp
        (error "`jit-spell-mode' requires `ispell-async-processp'"))
      (ispell-set-spellchecker-params)
      (ispell-internal-change-dictionary)
      (setq proc (ispell-start-process))
      (set-process-query-on-exit-flag proc nil)
      (setf (alist-get params jit-spell--process-pool nil nil #'equal) proc)
      (set-process-filter proc #'jit-spell--process-filter)
      (set-process-buffer proc (generate-new-buffer " *jit-spell*"))
      (process-send-string proc "!\n")  ;Enter terse mode
      proc)))

(defun jit-spell--process-filter (proc string)
  "Filter function for jit-spell processes."
  (with-current-buffer (process-buffer proc)
    (insert string)
    (when (and (eq (char-before) ?\n) (eq (pos-bol 0) (1- (point))))
      (pcase-let ((`(,buffer ,tick ,start ,end)
                   (process-get proc 'jit-spell--current-request)))
        (process-put proc 'jit-spell--current-request nil)
        (cond ((not (buffer-live-p buffer)))
              ((not (eq tick (buffer-chars-modified-tick buffer)))
               ;; Got a belated response, so schedule a retry
               (jit-spell--schedule-pending-checks buffer))
              (t ;; Response is good, apply misspelling overlays
               (let (misspellings)
                 (goto-char (point-min))
                 (while (re-search-forward
                         (rx bol
                             (or (seq (any "&?")
                                      " " (group-n 1 (+ (not " ")))
                                      " " (+ digit)
                                      " " (group-n 2 (+ digit))
                                      ":")
                                 (seq "#"
                                      " " (group-n 1 (+ (not " ")))
                                      " " (group-n 2 (+ digit)))))
                         nil t)
                   (let ((word (match-string 1))
                         (start (string-to-number (match-string 2)))
                         corrections)
                     (goto-char (match-end 0))
                     (while (re-search-forward (rx (+ (not (any ", \n"))))
                                               (pos-eol) t)
                       (push (match-string 0) corrections))
                     (push (list word start (nreverse corrections))
                           misspellings)))
                 (jit-spell--make-overlays buffer start end misspellings))))
        (delete-region (point-min) (point-max))
        ;; Send next request to ispell process, if applicable
        (catch 'jit-spell--done
          (while-let ((request (pop (process-get proc 'jit-spell--requests)))
                      (buffer (car request)))
            (when (buffer-live-p buffer)
              (if (not (eq (cadr request) (buffer-chars-modified-tick buffer)))
                  (jit-spell--schedule-pending-checks buffer)
                (jit-spell--send-request proc request)
                (throw 'jit-spell--done nil)))))))))

(defun jit-spell--send-request (proc request)
  "Send REQUEST to ispell process PROC."
  (process-put proc 'jit-spell--current-request request)
  (pcase-let ((`(,buffer _ ,start ,end) request))
    (with-current-buffer buffer
      (let ((text (buffer-substring-no-properties start end)))
        (process-send-string proc "^")
        (process-send-string proc text)
        (process-send-string proc "\n")))))

(defun jit-spell--schedule-pending-checks (buffer)
  "Schedule a call to `jit-spell--check-pending-regions' in BUFFER."
  (with-current-buffer buffer
    (when (timerp jit-spell--recheck-timer)
      (cancel-timer jit-spell--recheck-timer))
    (setq jit-spell--recheck-timer
          (run-with-idle-timer 0.1 nil #'jit-spell--check-pending-regions buffer))))

(defun jit-spell--check-pending-regions (buffer)
  "Enqueue spell check requests for all pending regions of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer         ;TODO: Need to widen?
      (let ((proc (jit-spell--get-process))
            (tick (buffer-chars-modified-tick))
            (end (point-min))
            (limit (point-max))
            start)
        (while (setq start (text-property-any end limit 'jit-spell-pending t))
          (setq end (or (text-property-not-all start limit 'jit-spell-pending t)
                        limit))
          (push (list buffer tick start end) (process-get proc 'jit-spell--requests)))
        (when-let ((request (and (not (process-get proc 'jit-spell--current-request))
                                 (pop (process-get proc 'jit-spell--requests)))))
          (jit-spell--send-request proc request))))))

(defun jit-spell--filter-region (start end)
  "Return a list of subregions without control characters between START and END."
  (goto-char start)
  (let (regions)
    (while (re-search-forward (rx (+ (not cntrl))) end t)
      (push (cons (match-beginning 0) (match-end 0)) regions))
    regions))

;;; Mode-specific support

(defun jit-spell--has-face-p (faces v)
  "Non-nil if V, a face or list of faces, includes any of the FACES."
  (if (listp v)
      (seq-some (lambda (f) (memq f faces)) v)
    (memq v faces)))

(defun jit-spell--default-ignored-p (start end)
  "Return non-nil if word between START and END should not be spell-checked."
  (or (get-text-property start 'jit-spell-ignored)
      (jit-spell--has-face-p jit-spell-ignored-faces
                             (get-char-property start 'face))
      (member (buffer-substring-no-properties start end)
              jit-spell--local-words)))

(defun jit-spell--refine-by-face (faces &optional only)
  "Return a function to refine a list of regions based on its faces.
If ONLY is nil, regions containing any of the FACES are excluded.
Otherwise, only such regions are kept."
  (let ((pred (if only
                  #'jit-spell--has-face-p
                (lambda (faces v) (not (jit-spell--has-face-p faces v))))))
    (lambda (regions)
      (mapcan
       (pcase-lambda (`(,start . ,end)) ;; Refine one region
         (let (result)
           (with-restriction start end
             (goto-char start)
             (while-let ((prop (text-property-search-forward 'face faces pred)))
               (push `(,(prop-match-beginning prop) . ,(prop-match-end prop))
                     result)))
           (jit-spell--remove-overlays start end result)
           result))
       regions))))

(defun jit-spell--apostrophe-hack (regions)
  "Refine REGIONS to work around Hunspell's apostrophe issue."
  (mapcan
   (pcase-lambda (`(,i . ,end)) ;; Refine one region
     (let (result)
       (goto-char i)
       (while (re-search-forward (rx (? (group alpha)) (group (+ ?'))) end t)
         (unless (and (match-beginning 1) (looking-at-p (rx alpha)))
           (push (cons i (match-beginning 2)) result)
           (setq i (point))))
       (push (cons i end) result)))
   regions))

(defun jit-spell--check-region (start end)
  "Enqueue a spell check request for region between START and END.
This is intended to be a member of `jit-lock-functions'."
  (save-excursion
    ;; Extend region to include whole words
    (setq start (progn (goto-char start)
                       (re-search-backward (rx (or blank bol)))
                       (match-end 0)))
    (setq end (progn (goto-char end)
                     (re-search-forward (rx (or blank eol)))
                     (match-beginning 0)))
    (let ((proc (jit-spell--get-process))
          (buffer (current-buffer))
          (tick (buffer-chars-modified-tick)))
      (pcase-dolist (`(,i . ,j) (funcall jit-spell--filter-region start end))
        (put-text-property i j 'jit-spell-pending t)
        (let ((request (list buffer tick i j)))
          (if (process-get proc 'jit-spell--current-request)
              (push request (process-get proc 'jit-spell--requests))
            (jit-spell--send-request proc request)))))
    `(jit-lock-bounds ,start . ,end)))

;;; Interactive commands

(defun jit-spell--accept-word (word where)
  "Accept spelling of WORD.
WHERE can be `dict' (save in personal dictionary), `buffer' (save
as a local word at the end of the buffer), `session' (accept
temporarily and in this buffer only), or `query' (ask for one of
the above)."
  (pcase-exhaustive where
    ('session (when ispell-buffer-local-name
                (setq ispell-buffer-local-name (buffer-name)))
              (cl-pushnew word jit-spell--local-words
                          :test #'equal))
    ('buffer (ispell-add-per-file-word-list word)
             (jit-spell--accept-word word 'session))
    ('dict (process-send-string (jit-spell--get-process)
                                (format "*%s\n#\n" word)))
    ('query (jit-spell--accept-word
             word
             (pcase (read-multiple-choice
                     (substitute-quotes (format "Add `%s' to" word))
                     '((?d "dictionary" "Save this word in your personal dictionary.")
                       (?b "buffer" "Add a local word at the end of this buffer.")
                       (?s "session" "Accept this spelling temporarily and in this buffer only.")))
               (`(?d . ,_) 'dict)
               (`(?b . ,_) 'buffer)
               (`(?s . ,_) 'session)))))
  (jit-lock-refontify))

(defun jit-spell-correct-word (arg)
  "Correct a misspelled word in the selected window.

You can also accept the spelling in question by entering `@' in
the prompt.  To modify the spelling to be accepted, say change
capitalization or inflection, enter any text after the `@'.

In the minibuffer, cycle between the visible misspellings.  There
command is bound to `jit-spell-correct-next-key'.

With a numeric ARG, move backwards that many misspellings."
  (interactive "p")
  (if (minibufferp)
      (throw 'jit-spell--next arg)
    (named-let recur ((count (- arg)) (pos (point)))
      (let* ((ov (or (jit-spell--search-overlay pos count)
                     (user-error "No more misspellings")))
             (start (overlay-start ov))
             (end (overlay-end ov))
             (word (buffer-substring-no-properties start end))
             (highlight (make-overlay start end))
             (key (if (eq jit-spell-correct-next-key 'auto)
                      (where-is-internal 'jit-spell-correct-word nil t)
                    jit-spell-correct-next-key))
             (prompt (format-prompt "Correct `%s' (`@' to accept)" nil word))
             (cands (append (overlay-get ov 'jit-spell-corrections)
                            (list (concat "@" word))))
             (metadata '(metadata (category . jit-spell)
                                  (cycle-sort-function . identity)
                                  (display-sort-function . identity)))
             (coll (lambda (string pred action)
                     (if (eq action 'metadata)
                         metadata
                       (complete-with-action action cands string pred)))))
        (overlay-put highlight 'face 'highlight)
        (pcase (catch 'jit-spell--next
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (when-let ((map (and key (make-sparse-keymap))))
                         (set-keymap-parent map (current-local-map))
                         (define-key map key 'jit-spell-correct-word)
                         (use-local-map map)))
                   (unwind-protect
                       (completing-read prompt coll nil nil nil nil nil t)
                     (delete-overlay highlight))))
          ((and arg (pred numberp))
           (recur (* (cl-signum count) arg) (overlay-start ov)))
          ((and corr (rx bos ?@ (* ?\s) (? (group (+ nonl)))))
           (jit-spell--accept-word (or (match-string 1 corr) word) 'query))
          (corr (jit-spell--apply-correction ov corr)))))))

(defun jit-spell-remove-word (word)
  "Remove the WORD from personal dictionary or local word lists.
Interactively offer the current word or sub-words."
  (interactive (list (let ((cand (current-word)))
                       (completing-read
                        "Remove word: "
                        (append `(,cand) (string-split cand (rx (not alpha))))
                        nil nil nil nil cand))))
  (let ((places nil))
    ;; Check among the local words.
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (rx bol (syntax comment-start) (*? nonl)
                                   (literal ispell-words-keyword) (*? nonl)
                                   (group-n 1 (and " " (literal word))))
                               nil t)
        (push "the buffer's word list" places)
        (delete-region (match-beginning 1) (match-end 1))))
    ;; Remove it from the local word list.
    (when (member word jit-spell--local-words)
      (cl-callf2 delete word jit-spell--local-words)
      (unless places (push "the session's word list" places)))
    ;; Remove it from the personal dictionary
    (let ((case-fold-search nil))
      (with-temp-buffer
        (insert-file-contents ispell-personal-dictionary nil nil nil t)
        (when (re-search-forward (rx bol (literal word)) nil t)
          (delete-line)
          (let ((inhibit-message t))
            (write-file ispell-personal-dictionary))
          (push "the personal dictionary" places)
          (let* ((proc (jit-spell--get-process))
                 (pending (process-get proc 'jit-spell--requests)))
            (kill-process proc)
            (mapc (pcase-lambda (`(,buf _ _ _))
                    (with-current-buffer buf (jit-lock-refontify)))
                  pending)))))
    (if (not places)
        (message
         "\"%s\" not found in the personal dictionary or local word lists."
         word)
      (message "\"%s\" removed from %s." word (string-join places " and "))
      (jit-lock-refontify))))

(defalias 'jit-spell-change-dictionary 'ispell-change-dictionary) ;For discoverability

;;; Minor mode definition

(defun jit-spell--context-menu (menu click)
  "Context menu for `jit-spell-mode'.
MENU and CLICK are as expected of a member of `context-menu-functions'.
It can also be bound to a mouse click to pop up the menu."
  (interactive "i\ne")
  (save-excursion
    (mouse-set-point click)
    (when-let ((ov (jit-spell--overlay-at (point)))
               (word (buffer-substring-no-properties
                  (overlay-start ov) (overlay-end ov)))
               (map (or menu (make-sparse-keymap))))
      (dolist (corr (overlay-get ov 'jit-spell-corrections))
        (easy-menu-add-item map '("Correct Word")
                            (vector corr (lambda () (interactive)
                                           (jit-spell--apply-correction ov corr)))))
      (easy-menu-add-item map nil `["Save to Dictionary"
                                    (jit-spell--accept-word ,word 'dict)])
      (easy-menu-add-item map nil `["Save to Buffer"
                                    (jit-spell--accept-word ,word 'buffer)])
      (easy-menu-add-item map nil `["Accept for Session"
                                    (jit-spell--accept-word ,word 'session)])
      (unless menu (popup-menu map)))
    menu))

(defun jit-spell--read-local-words ()
  "Look for local words in the buffer and accept them for this session."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ispell-words-keyword nil t)
      (let ((limit (pos-eol)))
	(while (re-search-forward "\\s-*\\(\\S-+\\)" limit t)
          (jit-spell--accept-word (match-string-no-properties 1) 'session))))))

;;;###autoload
(define-minor-mode jit-spell-mode
  "Just-in-time spell checking."
  :keymap (make-sparse-keymap)
  :lighter (" Spell"
            (:propertize
             (:eval
              (let ((s (or ispell-local-dictionary ispell-dictionary)))
                (if s (concat "/" (substring s 0 (string-search "_" s))))))))
  (cond
   (jit-spell-mode
    ;; Major mode support
    (cond
     ((derived-mode-p 'prog-mode)
      (add-function :filter-return (local 'jit-spell--filter-region)
                    (jit-spell--refine-by-face jit-spell-prog-mode-faces t)))
     ((derived-mode-p 'tex-mode 'context-mode)
      (add-function :filter-return (local 'jit-spell--filter-region)
                    (jit-spell--refine-by-face jit-spell-tex-ignored-faces))))
    ;; Generic ignore predicate
    (when-let ((pred (or (bound-and-true-p flyspell-generic-check-word-predicate)
                         (get major-mode 'flyspell-mode-predicate))))
      (add-function :after-until (local 'jit-spell--ignored-p)
                    (lambda (_start end) (ignore-errors
                                           (save-excursion
                                             (goto-char end)
                                             (not (funcall pred)))))))
    ;; Hunspell workaround
    (when (if (eq 'auto jit-spell-use-apostrophe-hack)
              ispell-really-hunspell
            jit-spell-use-apostrophe-hack)
      (add-function :filter-return (local 'jit-spell--filter-region)
                    #'jit-spell--apostrophe-hack))
    ;; Buffer setup
    (jit-spell--read-local-words)
    (add-hook 'ispell-change-dictionary-hook 'jit-spell--unfontify nil t)
    (add-hook 'context-menu-functions 'jit-spell--context-menu nil t)
    (add-hook 'post-command-hook #'jit-spell--unhide-overlay-maybe nil t)
    ;; Font-lock needs to run first
    (add-hook 'jit-lock-functions #'jit-spell--check-region 10 t)
    (jit-lock-register #'jit-spell--check-region))
   (t
    (jit-lock-unregister #'jit-spell--check-region)
    (remove-hook 'post-command-hook #'jit-spell--unhide-overlay-maybe t)
    (remove-hook 'context-menu-functions 'jit-spell--context-menu t)
    (remove-hook 'ispell-change-dictionary-hook 'jit-spell--unfontify t)
    (kill-local-variable 'jit-spell--local-words)
    (kill-local-variable 'jit-spell--filter-region)
    (kill-local-variable 'jit-spell--ignored-p)
    (jit-spell--unfontify nil nil t))))

;; Don't litter M-x
(put 'jit-spell--context-menu 'completion-predicate #'ignore)
(dolist (sym '(jit-spell-change-dictionary
               jit-spell-correct-word))
  (put sym 'completion-predicate
       (lambda (_ buffer)
         (buffer-local-value 'jit-spell-mode buffer))))

(provide 'jit-spell)

; LocalWords:  jit ispell
;;; jit-spell.el ends here
