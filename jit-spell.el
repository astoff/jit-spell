;;; jit-spell.el --- Just-in-time spell checking      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: tools, wp
;; URL: https://github.com/astoff/jit-spell
;; Package-Requires: ((emacs "27.1") (compat "29.1"))
;; Version: 0

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

;; TODO

;;; Code:

(require 'compat)
(require 'ispell)
(require 'seq)
(eval-when-compile (require 'subr-x))

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
  '(font-latex-math-face
    font-latex-sedate-face
    message-header-name)
  "Faces jit-spell should ignore."
  :type '(repeat symbol))

(defcustom jit-spell-prog-mode-faces
  '(font-lock-comment-face
    font-lock-doc-face
    font-lock-string-face)
  "Faces jit-spell should check in modes derived from `prog-mode'."
  :type '(repeat symbol))

(defvar jit-spell-delayed-commands
  '(backward-delete-char-untabify
    self-insert-command)
  "List of commands with delayed spell checking.
Wait for `jit-spell-current-word-delay' seconds before
highlighting a misspelling at point after one of these commands.")

(defvar jit-spell-ignored-p #'jit-spell--default-ignored-p
  "Predicate satisfied by words to ignore.
It should be a function taking two arguments, the start and end
positions of the word.")

(defvar jit-spell--process-pool nil
  "A collection of ispell processes for `jit-spell-mode'.")

(defvar jit-spell--hidden-overlay nil
  "Place to keep track of a hidden overlay near the cursor.")

(defvar-local jit-spell--recheck-timer nil
  "Timer to debounce recheck requests.")

;;; Mode-specific support

(defun jit-spell--default-ignored-p (start end)
  "Return non-nil if word between START and END should not be spell-checked."
  (or (get-text-property start 'jit-spell-ignored)
      (let ((face (get-text-property start 'face)))
        (if (listp face)
            (seq-some (lambda (f) (memq f jit-spell-ignored-faces))
                      face)
          (memq face jit-spell-ignored-faces)))
      (member (buffer-substring-no-properties start end)
              ispell-buffer-session-localwords)))

(defun jit-spell--prog-ignored-p (start _end)
  "Additional ignore predicate for `prog-mode'."
  (let ((face (get-text-property start 'face)))
    (not (if (listp face)
             (seq-some (lambda (f) (memq f jit-spell-prog-mode-faces))
                       face)
           (memq face jit-spell-prog-mode-faces)))))

;; TODO: jit-spell--org-ignored-p, etc.

;;; Overlays

(put 'jit-spell 'evaporate t)

(defun jit-spell--make-overlay (start end corrections)
  "Make an overlay to highlight incorrect word between START and END.
Also add the list of CORRECTIONS as a property."
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'category 'jit-spell)
    (overlay-put ov 'jit-spell-corrections corrections)
    (if (not (<= start (point) end))
        (overlay-put ov 'face 'jit-spell-misspelling)
      (jit-spell--unhide-overlay)
      (setq jit-spell--hidden-overlay
            `(,(run-with-timer jit-spell-current-word-delay nil
                               #'jit-spell--unhide-overlay)
              ,ov
              jit-spell-misspelling)))
    ov))

(defun jit-spell--make-overlays (buffer start end misspellings)
  "Create jit-spell overlays in BUFFER between START and END.
MISSPELLINGS is a list with elements consisting of a word, a
character offset from START, and a list of corrections."
  (with-current-buffer buffer
    (with-silent-modifications
      (remove-list-of-text-properties start end '(jit-spell-pending))
      (remove-overlays start end 'category 'jit-spell)
      (pcase-dolist (`(,word ,offset ,corrections) misspellings)
        (let* ((wstart (+ start offset -1))
               (wend (+ wstart (length word))))
          (unless (funcall jit-spell-ignored-p wstart wend)
            (jit-spell--make-overlay wstart wend corrections)))))))

(defun jit-spell--overlay-at (pos)
  "Return the jit-spell overlay at POS, if it exists."
  (seq-some (lambda (ov)
              (and (eq (overlay-get ov 'category) 'jit-spell) ov))
            (overlays-at pos)))

(defun jit-spell--search-overlay (pos count)
  "Return the COUNT jit-spell overlay from POS."
  (let* ((limit (if (> count 0) (window-end) (window-start)))
         (searchfn (if (> count 0)
                       #'next-single-char-property-change
                     #'previous-single-char-property-change))
         (i (abs count)))
    (catch 'jit-spell
      (while (/= pos limit)
        (setq pos (funcall searchfn pos 'category nil limit))
        (dolist (ov (overlays-at pos))
          (when (eq (overlay-get ov 'category) 'jit-spell)
            (cl-decf i)
            (unless (< 0 i)
              (throw 'jit-spell ov))))))))

(defun jit-spell--apply-correction (ov text)
  "Replace region spanned by OV with TEXT."
  (save-excursion
    (goto-char (overlay-start ov))
    (delete-region (point) (overlay-end ov))
    (insert-before-markers text)))

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
                                    (jit-spell-accept-word ,word 'dict)])
      (easy-menu-add-item map nil `["Save to Buffer"
                                    (jit-spell-accept-word ,word 'buffer)])
      (easy-menu-add-item map nil `["Accept for Session"
                                    (jit-spell-accept-word ,word 'session)])
      (unless menu (popup-menu map)))
    menu))

(defun jit-spell--unhide-overlay ()
  "Unhide the overlay stored in `jit-spell--hidden-overlay'."
  (pcase jit-spell--hidden-overlay
    (`(,timer ,ov ,face)
     (cancel-timer timer)
     (overlay-put ov 'face face)
     (setq jit-spell--hidden-overlay nil))))

(defun jit-spell--unfontify (&optional start end)
  "Remove overlays and forget checking status from START to END (or whole buffer)."
  (save-restriction
    (widen)
    (setq start (or start (point-min)))
    (setq end (or end (point-max)))
    (remove-overlays start end 'category 'jit-spell)
    (remove-list-of-text-properties start end '(jit-spell-pending))))

;;; Subprocess communication

(defun jit-spell--process-parameters ()
  "Return a list of parameters for this buffer's ispell process.
Buffers where this produces `equal' results will share their
ispell process."
  (list ispell-program-name
        ispell-current-dictionary
        ispell-current-personal-dictionary
        ispell-extra-args))

(defun jit-spell--get-process ()
  "Get an ispell process for the current buffer."
  (let* ((params (jit-spell--process-parameters))
         (proc (plist-get jit-spell--process-pool params #'equal)))
    (if (process-live-p proc)
        proc
      (unless ispell-async-processp
        (error "`jit-spell-mode' requires `ispell-async-processp'"))
      (ispell-set-spellchecker-params)
      (setq proc (ispell-start-process))
      (set-process-query-on-exit-flag proc nil)
      (setq jit-spell--process-pool
            (plist-put jit-spell--process-pool params proc #'equal))
      (set-process-filter proc #'jit-spell--process-filter)
      (set-process-buffer proc (generate-new-buffer " *jit-spell*"))
      (process-send-string proc "!\n")  ;Enter terse mode
      proc)))

(defun jit-spell--process-filter (proc string)
  "Filter function for jit-spell processes."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (when (re-search-forward "^\n" nil t) ;TODO: Process in chunks
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
        (let (request)
          (while (and (setq request (pop (process-get proc 'jit-spell--requests)))
                      (pcase-let ((`(,buffer ,tick) request))
                        (not (and (buffer-live-p buffer)
                                  (eq tick (buffer-chars-modified-tick buffer))))))
            (when (buffer-live-p buffer)
              (jit-spell--schedule-pending-checks (car request))))
          (when request
            (jit-spell--send-request proc request)))))))

(defun jit-spell--send-request (proc request)
  "Send REQUEST to ispell process PROC."
  (process-put proc 'jit-spell--current-request request)
  (pcase-let ((`(,buffer _ ,start ,end) request))
    (with-current-buffer buffer
      (let ((text (buffer-substring-no-properties start end)))
                                        ;TODO: allow custom
                                        ;buffer-substring functions
                                        ;e.g. to work around
                                        ;hunspell's apostrophe issue.
        ;; Redact control characters in text
        (dotimes (i (length text))
          (when (< (aref text i) ?\s)
            (aset text i ?\s)))
        (process-send-string proc "^")
        (process-send-string proc text)
        (process-send-string proc "\n")))))

(defun jit-spell--schedule-pending-checks (buffer)
  "Schedule a call to `jit-spell--check-pending-regions' in BUFFER."
  (when (buffer-live-p buffer)
    (when (bound-and-true-p jit-spell--debug)
      (message "Scheduled recheck"))
    (with-current-buffer buffer
      (when (timerp jit-spell--recheck-timer)
        (cancel-timer jit-spell--recheck-timer))
      (setq jit-spell--recheck-timer
            (run-with-idle-timer 0.1 nil #'jit-spell--check-pending-regions buffer)))))

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

(defun jit-spell--check-region (start end)
  "Enqueue a spell check request for region between START and END.
This is intended to be a member of `jit-lock-functions'."
  (save-excursion ;; Extend region to include whole words
    (goto-char start)
    (setq start (if (re-search-backward "\\s-" nil t) (match-end 0) (point-min)))
    (goto-char end)
    (setq end (or (re-search-forward "\\s-" nil t) (point-max))))
  (put-text-property start end 'jit-spell-pending t)
  (let ((proc (jit-spell--get-process))
        (request (list (current-buffer) (buffer-chars-modified-tick) start end)))
    (if (process-get proc 'jit-spell--current-request)
        (push request (process-get proc 'jit-spell--requests))
      (jit-spell--send-request proc request)))
  `(jit-lock-bounds ,start . ,end))

;;; Interactive commands and major mode

(defun jit-spell-accept-word (word where)
  "Accept spelling of WORD.
WHERE can be `dict' (save in personal dictionary), `buffer' (save
as a local word at the end of the buffer), `session' (accept
temporarily and in this buffer only), or `query' (ask for one of
the above)."
  (interactive
   (list (completing-read
          "Add word: "
          (thread-last
            (overlays-in (window-start) (window-end))
            (seq-sort (lambda (o1 o2) (< (overlay-start o1)
                                         (overlay-start o2))))
            (seq-keep (lambda (ov)
                        (when (eq (overlay-get ov 'category) 'jit-spell)
                          (buffer-substring-no-properties
                           (overlay-start ov) (overlay-end ov))))))
          nil nil nil nil
          (thing-at-point 'word))
         'query))
  (pcase-exhaustive where
    ('session (when ispell-buffer-local-name
                (setq ispell-buffer-local-name (buffer-name)))
              (cl-pushnew word ispell-buffer-session-localwords
                          :test #'equal))
    ('buffer (ispell-add-per-file-word-list word)
             (jit-spell-accept-word word 'session))
    ('dict (process-send-string (jit-spell--get-process)
                                (format "*%s\n#\n" word)))
    ('query (jit-spell-accept-word
             word
             (pcase (read-multiple-choice (substitute-quotes
                                           (format "Add `%s' to" word))
                                          '((?d "dictionary")
                                            (?b "buffer")
                                            (?s "session"))) ;TODO: help string
               (`(?d ,_) 'dict)
               (`(?b ,_) 'buffer)
               (`(?s ,_) 'session)))))
  (jit-lock-refontify))

(defun jit-spell-correct-word--next (arg)
  "Perform a spooky action at a distance."
  (interactive "p")
  (throw 'jit-spell-correct-word--next arg))

(defun jit-spell-correct-word (arg &optional pos)
  "Correct a misspelled word in the selected window.

You can also accept the spelling in question by entering `@' in
the prompt.  It is possible to modify the spelling to be
accepted, say change capitalization or inflection, by entering
any text after the `@'.

With a numeric ARG, move backwards that many misspellings.
Alternatively, pressing \\<jit-spell-mode-map>\\[jit-spell-correct-word] \
again moves to the next misspelling."
  (interactive "p")
  (let* ((ov (or (jit-spell--search-overlay (or pos (point)) (- arg))
                 (user-error "No more misspellings")))
         (start (overlay-start ov))
         (end (overlay-end ov))
         (word (buffer-substring-no-properties start end))
         (highlight (make-overlay start end))
         (map (make-sparse-keymap))
         (prompt (format-prompt "Correct `%s' (`@' to accept)" nil word))
         (coll (append (overlay-get ov 'jit-spell-corrections)
                       (list (concat "@" word)))))
    (dolist (key (where-is-internal 'jit-spell-correct-word))
      (define-key map key 'jit-spell-correct-word--next))
    (overlay-put highlight 'face 'highlight)
    (pcase (catch 'jit-spell-correct-word--next
             (minibuffer-with-setup-hook
                 (lambda ()
                   (set-keymap-parent map (current-local-map))
                   (use-local-map map))
               (unwind-protect
                   (completing-read prompt coll nil nil nil nil nil t)
                 (delete-overlay highlight))))
      ((and count (pred numberp))
       (jit-spell-correct-word count (overlay-start ov)))
      ((and corr (rx bos ?@ (* space) (? (group (+ nonl)))))
       (jit-spell-accept-word (or (match-string 1 corr) word) 'query))
      (corr (jit-spell--apply-correction ov corr)))))

(defalias 'jit-spell-change-dictionary 'ispell-change-dictionary) ;For discoverability

(defun jit-spell--read-local-words ()
  "Look for local words in the buffer and accept them for this session."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ispell-words-keyword nil t)
      (let ((limit (pos-eol)))
	(while (re-search-forward "\\s-*\\(\\S-+\\)" limit t)
          (jit-spell-accept-word (match-string-no-properties 1) 'session))))))

(defun jit-spell--pre-command-hook ()
  "Pre-command hook for `jit-spell-mode'."
  (when (and jit-spell--hidden-overlay
             (not (memq this-command jit-spell-delayed-commands)))
    (jit-spell--unhide-overlay)))

(defvar-keymap jit-spell-mode-map
  :doc "Keymap for `jit-spell-mode'."
  "C-;" #'jit-spell-correct-word
  "C-:" #'jit-spell-accept-word)

;;;###autoload
(define-minor-mode jit-spell-mode
  "Just-in-time spell checking."
  :keymap jit-spell-mode-map
  :lighter (" Spell"
            (:propertize
             (:eval
	      (concat "/" (let ((s (or ispell-local-dictionary
			               ispell-dictionary
                                       "--")))
                            (substring s 0 (string-search "_" s)))))
             help-echo "mouse-1: Change dictionary"
             local-map (keymap
                        (mode-line keymap
                                   (mouse-1 . ispell-change-dictionary)))))
  (cond
   (jit-spell-mode
    (cond
     ((derived-mode-p 'prog-mode)
      (add-function :before-until (local 'jit-spell-ignored-p)
                    #'jit-spell--prog-ignored-p))
     ((derived-mode-p 'org-mode)
      (setq-local jit-spell-delayed-commands
                  (append '(org-delete-backward-char org-self-insert-command)
                          jit-spell-delayed-commands))))
    (jit-spell--read-local-words)
    (add-hook 'ispell-change-dictionary-hook 'jit-spell--unfontify nil t)
    (add-hook 'context-menu-functions 'jit-spell--context-menu nil t)
    (add-hook 'pre-command-hook #'jit-spell--pre-command-hook nil t)
    (jit-lock-register #'jit-spell--check-region))
   (t
    (jit-lock-unregister #'jit-spell--check-region)
    (remove-hook 'pre-command-hook #'jit-spell--pre-command-hook)
    (remove-hook 'context-menu-functions 'jit-spell--context-menu t)
    (remove-hook 'ispell-change-dictionary-hook 'jit-spell--unfontify t)
    (kill-local-variable 'ispell-buffer-session-localwords)
    (kill-local-variable 'jit-spell-delayed-commands)
    (kill-local-variable 'jit-spell-ignored-p)))
  (jit-spell--unfontify))

;; Don't litter M-x
(put 'jit-spell--context-menu 'completion-predicate #'ignore)
(dolist (sym '(jit-spell-change-dictionary
               jit-spell-correct-word
               jit-spell-accept-word))
  (put sym 'completion-predicate (lambda (&rest _) jit-spell-mode)))

(provide 'jit-spell)

;;; jit-spell.el ends here
