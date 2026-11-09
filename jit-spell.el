;;; jit-spell.el --- Just-in-time spell checking      -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: text
;; URL: https://github.com/astoff/jit-spell
;; Package-Requires: ((emacs "28.1") (compat "31.1"))
;; Version: 0.6

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

;;; News:

;; Version 0.6
;; - Add `jit-spell-dictionary' variable and rename the
;;  `jit-spell-change-dictionary' to `jit-spell-dictionary'.
;; - Define `jit-spell-engine' and add support for vibe-spell.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'ispell)
(require 'text-property-search)
(require 'vibe-spell nil t)

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

(defcustom jit-spell-engine (if (featurep 'vibe-spell)
                                'vibe-spell 'ispell)
  "The spell-checking engine to use.
Can be either `vibe-spell', which is recommended but is an external
package, or `ispell', which is built-in."
  :type '(choice (const vibe-spell) (const ispell)))

(defcustom jit-spell-dictionary nil
  "One or more dictionaries used for spellchecking.
Can be a string or list of strings."
  :type '(choice string (repeat string))
  :safe (lambda (v) (or (stringp v) (all #'stringp v))))

(defvar jit-spell--ignored-p #'jit-spell--default-ignored-p
  "Predicate satisfied by words to ignore.
It should be a function taking one argument, the word starting position.
Point is placed just after the word.  This function is also allowed to
advance the point to signify that checking should resume from that
point.")

(defvar jit-spell--hidden-overlay nil
  "Place to keep track of a hidden overlay near the cursor.")

(defvar-local jit-spell--recheck-timer nil
  "Timer to debounce recheck requests.")

(defvar-local jit-spell--local-words nil
  "A list of words accepted temporarily in this buffer.")

;; For the byte-compiler
(defvar jit-spell-mode)

;;; Overlays

(put 'jit-spell 'evaporate t)
(put 'jit-spell 'face 'jit-spell-misspelling)

(defun jit-spell--make-overlay (start end &optional opoint)
  "Make an overlay to highlight incorrect word between START and END.
OPOINT is the user's original point position, used to decide whether to
hide the overlay while editing the word."
  (let ((ov (make-overlay start end nil t)))
    (if (not (<= start (or opoint (point)) end))
        (overlay-put ov 'category 'jit-spell)
      (jit-spell--unhide-overlay)
      (setq jit-spell--hidden-overlay
            (cons ov (run-with-timer jit-spell-current-word-delay nil
                                     #'jit-spell--unhide-overlay))))
    ov))

(defun jit-spell--overlay-at (pos)
  "Return the jit-spell overlay at POS, if it exists."
  (car (any (lambda (ov)
              (eq (overlay-get ov 'category) 'jit-spell))
            (overlays-at pos))))

(defun jit-spell--search-overlay (pos count)
  "Return the COUNT jit-spell overlay from POS."
  (let* ((limit (if (plusp count) (window-end) (window-start)))
         (searchfn (if (plusp count)
                       #'next-single-char-property-change
                     #'previous-single-char-property-change))
         (i (abs count)))
    (catch 'jit-spell
      (while (/= pos limit)
        (setq pos (funcall searchfn pos 'category nil limit))
        (unless (invisible-p pos)
          (when-let* ((ov (jit-spell--overlay-at pos)))
            (unless (plusp (decf i))
              (throw 'jit-spell ov))))))))

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
  (when-let* ((ov (car jit-spell--hidden-overlay)))
    (unless (and (eq (overlay-buffer ov) (current-buffer))
                 (save-excursion
                   (re-search-forward (rx (or blank eol)))
                   (<= (overlay-start ov) (match-beginning 0)))
                 (save-excursion
                   (re-search-backward (rx (or blank bol)))
                   (<= (match-end 0) (overlay-end ov))))
      (jit-spell--unhide-overlay))))

;;; Spell-checking engine interface

(cl-defgeneric jit-spell--schedule (request)
  "Schedule spell-checking of a region.
REQUEST is a list of the form (BUFFER TICK START END), where TICK is
used to keep track of the timeliness of the response.")

(cl-defgeneric jit-spell--drop-buffer (buffer)
  "Forget all pending checks on BUFFER.")

(cl-defgeneric jit-spell--get-suggestions (overlay)
  "Get correction suggestions for word covered by OVERLAY.")

(cl-defgeneric jit-spell--save-word (word)
  "Save WORD in the personal dictionary.")

(cl-defgeneric jit-spell--delete-word (word)
  "Delete WORD from the personal dictionary.")

(cl-defgeneric jit-spell--list-dictionaries ()
  "Return a list of available dictionaries.")

(defun jit-spell--verify-or-reschedule (request)
  "Check if REQUEST is still applicable.
Return non-nil if the buffer is alive and its char modification tick
hasn't change since REQUEST was created.  Otherwise, schedule a call to
`jit-spell--check-pending-regions'."
  (pcase-let ((`(,buffer ,tick) request))
    (when (buffer-live-p buffer)
      (or (eq tick (buffer-chars-modified-tick buffer))
          ;; Got a belated response, so schedule a retry
          (with-current-buffer buffer
            (when (timerp jit-spell--recheck-timer)
              (cancel-timer jit-spell--recheck-timer))
            (setq jit-spell--recheck-timer
                  (run-with-idle-timer
                   0.1 nil #'jit-spell--check-pending-regions buffer))
            nil)))))

(defun jit-spell--check-pending-regions (buffer)
  "Schedule spell-check requests for all pending regions of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer         ;TODO: Need to widen?
      (let ((tick (buffer-chars-modified-tick))
            (end (point-min))
            (limit (point-max))
            start)
        (while (setq start (text-property-any end limit 'jit-spell-pending t))
          (setq end (or (text-property-not-all start limit 'jit-spell-pending t)
                        limit))
          (jit-spell--schedule (list buffer tick start end)))))))

(defun jit-spell--check-region (start end)
  "Schedule spell-check requests for region between START and END.
This is intended to be a member of `jit-lock-functions'."
  (save-excursion
    ;; Extend region to include whole words
    (setq start (progn (goto-char start)
                       (re-search-backward (rx (or blank bol)))
                       (match-end 0)))
    (setq end (progn (goto-char end)
                     (re-search-forward (rx (or blank eol)))
                     (match-beginning 0)))
    (let ((buffer (current-buffer))
          (tick (buffer-chars-modified-tick)))
      (put-text-property start end 'jit-spell-pending t)
      (jit-spell--schedule (list buffer tick start end)))
    `(jit-lock-bounds ,start . ,end)))

(defun jit-spell--unfontify (&optional start end lax)
  "Remove overlays and forget checking status from START to END (or whole buffer).
Force refontification of the region, unless LAX is non-nil."
  ;; Drop pending requests for this buffer
  (jit-spell--drop-buffer (current-buffer))
  (without-restriction
    (setq start (or start (point-min)))
    (setq end (or end (point-max)))
    (jit-spell--remove-overlays start end)
    (remove-list-of-text-properties start end '(jit-spell-pending))
    (unless lax (jit-lock-refontify start end))))

(defun jit-spell--dicts ()
  "Get a list of dictionaries for the current buffer."
  (or (ensure-list jit-spell-dictionary)
      (when-let* ((s (or ispell-local-dictionary ispell-dictionary)))
        (split-string s "," t))
      (when (bound-and-true-p current-locale-environment) ;Emacs 29
                 (list (replace-regexp-in-string
                        "\\..*" "" current-locale-environment)))
      (user-error "Unable to guess a dictionary, please set `jit-spell-dictionary'")))

;;; vibe-spell interface implementation

(declare-function vibe-spell-add-word "ext:vibe-spell")
(declare-function vibe-spell-check "ext:vibe-spell")
(declare-function vibe-spell-create "ext:vibe-spell")
(declare-function vibe-spell-list-dictionaries "ext:vibe-spell")
(declare-function vibe-spell-remove-word "ext:vibe-spell")
(declare-function vibe-spell-suggest "ext:vibe-spell")
(declare-function vibe-spell-word-regexp "ext:vibe-spell")

(defvar jit-spell--vspell-timer nil
  "Idle timer for next request to be fulfilled.")

(defvar jit-spell--vspell-queue nil
  "Queue of requests waiting to be fulfilled.")

(defvar jit-spell--vspell-dicts nil
  "Memoization place for function `jit-spell--vspell-dicts'.")

(defun jit-spell--vspell-dicts ()
  "Return a list of dictionaries for the current buffer."
  (mapcar (lambda (name)
            (with-memoization (alist-get name jit-spell--vspell-dicts
                                         nil nil #'string=)
              (vibe-spell-create name)))
          (jit-spell--dicts)))

(defun jit-spell--vspell-check (request)
  "Perform the actual checking of a given region.
REQUEST a list once passed to jit-spell--schedule`."
  (setq jit-spell--vspell-timer
        (when-let* ((next (pop jit-spell--vspell-queue)))
          (run-with-idle-timer
           0 nil #'jit-spell--vspell-check next)))
  (pcase-let ((`(,buffer ,_ ,start ,end) request))
    (when (jit-spell--verify-or-reschedule request)
      (with-current-buffer buffer
        (save-excursion
          (without-restriction
            (with-silent-modifications
              (let* ((dicts (jit-spell--vspell-dicts))
                     (opoint (point))
                     (re (vibe-spell-word-regexp (car dicts))))
                (remove-list-of-text-properties start end '(jit-spell-pending))
                (jit-spell--remove-overlays start end)
                (goto-char start)
                (while (re-search-forward re end t)
                  (let ((wstart (match-beginning 0))
                        (wend (match-end 0))
                        (word (match-string 0)))
                    (unless (or (funcall jit-spell--ignored-p wstart)
                                (any (lambda (dict)
                                       (vibe-spell-check dict word))
                                     dicts))
                      (jit-spell--make-overlay wstart wend opoint))))))))))))

(cl-defmethod jit-spell--schedule
    (request &context (jit-spell-engine (eql vibe-spell)))
  (if jit-spell--vspell-timer
      (cl-callf nconc jit-spell--vspell-queue (list request))
    (setq jit-spell--vspell-timer
          (run-with-idle-timer
           0 nil #'jit-spell--vspell-check request))))

(cl-defmethod jit-spell--drop-buffer
    (buffer &context (jit-spell-engine (eql vibe-spell)))
  ;; FIXME: How about current timer?
  (setq jit-spell--vspell-queue
        (seq-remove (lambda (r) (eq (car r) buffer))
                    jit-spell--vspell-queue)))

(cl-defmethod jit-spell--get-suggestions
    (ov &context (jit-spell-engine (eql vibe-spell)))
  (vibe-spell-suggest (car (jit-spell--vspell-dicts))
                      (buffer-substring-no-properties (overlay-start ov)
                                                      (overlay-end ov))))

(cl-defmethod jit-spell--save-word
    (word &context (jit-spell-engine (eql vibe-spell)))
  (vibe-spell-add-word (car (jit-spell--vspell-dicts)) word t))

(cl-defmethod jit-spell--delete-word
    (word &context (jit-spell-engine (eql vibe-spell)))
  (vibe-spell-remove-word (car (jit-spell--vspell-dicts)) word t))

(cl-defmethod jit-spell--list-dictionaries
    (&context (jit-spell-engine (eql vibe-spell)))
  (vibe-spell-list-dictionaries))

;;; ispell pipe interface implementation

(defvar jit-spell--ispell-process nil
  "Memoization place for function `jit-spell--ispell-process'.")

(defun jit-spell--ispell-process ()
  "Get an ispell process for the current buffer.
Buffers where `jit-spell--ispell-parameters' returns the `equal'
results share their ispell process.

The process plist includes `jit-spell--current-request', the REQUEST
currently being processed, and `jit-spell--requests', a LIFO queue of
upcoming requests."
  (require 'ispell)
  (let* ((params (list ispell-program-name
                       (jit-spell--dicts)
                       (or ispell-local-pdict ispell-personal-dictionary)
                       ispell-extra-args))
         (proc (cdr (assoc params jit-spell--ispell-process))))
    (if (process-live-p proc)
        proc
      (let* ((default-directory (expand-file-name "~/"))
             (process-environment (default-value 'process-environment))
             (exec-path (default-value 'exec-path))
             (dicts (string-join (cadr params) ","))
             (name (format "jit-spell-%s" dicts))
             (proc (make-process
                    :name name
                    :buffer (generate-new-buffer (format " *%s*" name))
                    :command `(,ispell-program-name
                               "-a" "-d" ,dicts
                               ,@(when (caddr params) `("-p" ,(caddr params)))
                               ,@ispell-extra-args)
                    :filter #'jit-spell--ispell-filter
                    :connection-type 'pipe
                    :noquery t)))
        (setf (alist-get params jit-spell--ispell-process nil nil #'equal) proc)
        (process-send-string proc "!\n")  ;Enter terse mode
        proc))))

(defun jit-spell--ispell-filter (proc string)
  "Filter function for ispell processes."
  (with-current-buffer (process-buffer proc)
    (insert string)
    (when (and (eq (char-before) ?\n) (eq (pos-bol 0) (1- (point))))
      (let ((request (process-get proc 'jit-spell--current-request)))
        (process-put proc 'jit-spell--current-request nil)
        (when (jit-spell--verify-or-reschedule request)
          (pcase-let ((`(,buffer ,_ ,start ,end) request)
                      (misspellings nil))
            ;; Parse ispell response
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
              (let* ((wstart (+ start (string-to-number (match-string 2)) -1))
                     (wend (+ wstart (- (match-end 1) (match-beginning 1))))
                     corrections)
                (goto-char (match-end 0))
                (while (re-search-forward (rx (+ (not (any ", \n"))))
                                          (pos-eol) t)
                  (push (match-string 0) corrections))
                (push `(,wstart ,wend ,@(nreverse corrections)) misspellings)))
            ;; Apply overlays
            (with-current-buffer buffer
              (save-excursion
                (without-restriction
                  (with-silent-modifications
                    (let ((opoint (point)))
                      (remove-list-of-text-properties start end '(jit-spell-pending))
                      (jit-spell--remove-overlays start end)
                      (pcase-dolist (`(,wstart ,wend . ,corrections)
                                     (nreverse misspellings))
                        (goto-char wend)
                        (unless (funcall jit-spell--ignored-p wstart)
                          (overlay-put (jit-spell--make-overlay wstart wend opoint)
                                       'jit-spell-corrections corrections)))))))))))
      (delete-region (point-min) (point-max))
      ;; Send next request to ispell process, if applicable
      (catch 'jit-spell--done
        (while-let ((request (pop (process-get proc 'jit-spell--requests))))
          (when (jit-spell--verify-or-reschedule request)
            (jit-spell--ispell-send proc request)
            (throw 'jit-spell--done nil)))))))

(defun jit-spell--ispell-send (proc request)
  "Send REQUEST to ispell process PROC."
  (process-put proc 'jit-spell--current-request request)
  (pcase-let ((`(,buffer _ ,start ,end) request))
    (with-current-buffer buffer
      (let ((text (buffer-substring-no-properties start end)))
        (process-send-string proc "^")
        (process-send-string proc text)
        (process-send-string proc "\n")))))

(defvar jit-spell--filter-region #'jit-spell--filter-region
  "Function to extract regions of interest from the buffer.
It receives the start and end positions of a region as argument,
and should return a list of cons cells representing subregions.

This function is called inside a `save-excursion' form and can
move the point with impunity.")

(cl-defmethod jit-spell--schedule
    (request &context (jit-spell-engine (eql ispell)))
  (pcase-let* ((proc (jit-spell--ispell-process))
               (`(,buffer ,tick ,start ,end) request)
               (regions (save-excursion
                          (funcall jit-spell--filter-region start end)))
               (requests (mapcar (pcase-lambda (`(,s . ,e))
                                   `(,buffer ,tick ,s ,e))
                                 regions)))
    ;; Hunspell workaround
    (when (if (eq 'auto jit-spell-use-apostrophe-hack)
              (string-search "hunspell" (car (process-command proc)))
            jit-spell-use-apostrophe-hack)
      (save-excursion
        (setq regions (jit-spell--apostrophe-hack regions))))
    ;; Remove pending property from the regions left out.
    (pcase-dolist (`(,s . ,e) regions)
      (remove-list-of-text-properties start s '(jit-spell-pending))
      (setq start e))
    (remove-list-of-text-properties start end '(jit-spell-pending))
    ;; Schedule the requests.
    (unless (process-get proc 'jit-spell--current-request)
      (jit-spell--ispell-send proc (pop requests)))
    (cl-callf nconc (process-get proc 'jit-spell--requests) requests)))

(cl-defmethod jit-spell--drop-buffer
    (buffer &context (jit-spell-engine (eql ispell)))
  (let ((proc (with-current-buffer buffer
                (jit-spell--ispell-process))))
    (setf (process-get proc 'jit-spell--requests)
          (seq-remove (lambda (r) (eq (car r) buffer))
                      (process-get proc 'jit-spell--requests)))
    (while (eq buffer (car (process-get proc 'jit-spell--current-request)))
      (accept-process-output proc))))

(cl-defmethod jit-spell--get-suggestions
    (ov &context (jit-spell-engine (eql ispell)))
  (overlay-get ov 'jit-spell-corrections))

(cl-defmethod jit-spell--save-word
    (word &context (jit-spell-engine (eql ispell)))
  (process-send-string (jit-spell--ispell-process)
                       (format "*%s\n#\n" word)))

(cl-defmethod jit-spell--delete-word
    (word &context (jit-spell-engine (eql ispell)))
  (when-let* ((proc (jit-spell--ispell-process))
              (dict (or ispell-local-pdict ispell-personal-dictionary))
              (_ (file-exists-p dict)))
    (with-temp-buffer
      (insert-file-contents dict)
      (let ((case-fold-search nil)
            (inhibit-message t))
        (when (re-search-forward (rx bol (literal word) eol) nil t)
          (delete-line)
          (write-file dict)
          (kill-process proc)
          t)))))

(cl-defmethod jit-spell--list-dictionaries
    (&context (jit-spell-engine (eql ispell)))
  (ispell-valid-dictionary-list))

;;;; Region filtering

(defun jit-spell--filter-region (start end)
  "Return a list of subregions without control characters between START and END."
  (goto-char start)
  (let (regions)
    (while (re-search-forward (rx (+ (not cntrl))) end t)
      (push (cons (match-beginning 0) (match-end 0)) regions))
    (nreverse regions)))

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

;;; Mode-specific support

(defun jit-spell--has-face-p (faces v)
  "Non-nil if V, a face or list of faces, includes any of the FACES."
  (if (listp v)
      (member-if (lambda (f) (memq f faces)) v)
    (memq v faces)))

(defun jit-spell--default-ignored-p (start)
  "Return non-nil if word at START should not be spell-checked.
See `jit-spell--ignored-p' for the usage conventions."
  (or (get-text-property start 'jit-spell-ignored)
      (jit-spell--has-face-p jit-spell-ignored-faces
                             (get-char-property start 'face))
      ;; Stuff like a Unix file name
      (when (eq (char-before start) ?/)
        (re-search-forward (rx (* (any alnum punct)))))
      ;; Stuff like email, URL or DOS file name
      (when (save-excursion
              (goto-char start)
              (re-search-forward (rx point
                                     (* (any alnum punct))
                                     (or "@" "://" ":\\")
                                     (* (any alnum punct)))
                                 nil t))
        (goto-char (match-end 0)))
      (let ((word (buffer-substring-no-properties start (point))))
        (member word jit-spell--local-words))))

;;; Interactive commands

(defun jit-spell--accept-word (word where)
  "Accept spelling of WORD.
WHERE can be `dictionary' (save in personal dictionary), `buffer' (save
as a local word at the end of the buffer), `session' (accept temporarily
and in this buffer only), or `query' (ask for one of the above)."
  (pcase-exhaustive where
    ('session (cl-pushnew word jit-spell--local-words :test #'equal))
    ('buffer (ispell-add-per-file-word-list word)
             (jit-spell--accept-word word 'session))
    ('dictionary (jit-spell--save-word word))
    ('query (jit-spell--accept-word
             word
             (intern (cadr (read-multiple-choice
                            (substitute-quotes (format "Add `%s' to" word))
                            '((?s "session" "\
Accept this spelling temporarily and in this buffer only.")
                              (?b "buffer" "\
Add a local word at the end of this buffer.")
                              (?d "dictionary" "\
Save this word in your personal dictionary."))))))))
  (jit-lock-refontify))

(defun jit-spell-correct-word (arg)
  "Correct a misspelled word in the selected window.

You can also accept the spelling in question by entering `@' in
the prompt.  To modify the spelling to be accepted, say change
capitalization or inflection, enter any text after the `@'.

In the minibuffer, cycle between the visible misspellings.  There
command is bound to `jit-spell-correct-next-key'.

With a numeric ARG, move backwards that many misspellings."
  (interactive "p" jit-spell-mode)
  (if (minibufferp)
      (throw 'jit-spell--next arg)
    (named-let recur ((count (- arg))
                      (pos (if (minusp arg)
                               (point)
                             ;; Catch misspelling at point (bug #6)
                             (min (1+ (point)) (point-max)))))
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
             (cands (append (jit-spell--get-suggestions ov)
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
                       (when-let* ((map (and key (make-sparse-keymap))))
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
Interactively offer the current word or sub-words.

To remove words from the personal dictionary `ispell-personal-dictionary'
must be set.  The location of personal dictionary depends on the spell checking
program and there is no reliable way of obtaining it."
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
    (when (jit-spell--delete-word word)
      (push "the personal dictionary" places))
    (if (not places)
        (message "\
\"%s\" not found in the personal dictionary or local word lists." word)
      (message "\"%s\" removed from %s." word (string-join places " and "))
      ;; FIXME: We might need to refontify other buffers as well.
      (jit-lock-refontify))))

(defun jit-spell-dictionary (dicts &optional where)
  "Set the jit-spell dictionaries in the current buffer to DICTS.
If WHERE is one of `file' or `directory', save the choice as a file-
respectively dir-local variable.

Interactively, use a prefix argument to ask WHERE."
  (interactive
   (reverse
    (list
     (when current-prefix-arg
       (intern (cadr (read-multiple-choice
                      "Set dictionaries"
                      '((?s "session" "\
Temporarily and in this buffer only.")
                        (?b "buffer" "\
Permanently as a file-local variable.")
                        (?d "directory" "\
Permanently as a directory-local variable."))))))
     (completing-read-multiple "Use dictionaries: "
                                   (jit-spell--list-dictionaries)
                                    nil t)))
   jit-spell-mode)
  (let ((doit (lambda ()
                (jit-spell--unfontify)
                (setq-local jit-spell-dictionary dicts))))
    (pcase where
      ((or 'nil 'session) (funcall doit))
      ('buffer
       (save-excursion
         (add-file-local-variable 'jit-spell-dictionary dicts))
       (funcall doit))
      ('directory
       (add-dir-local-variable nil 'jit-spell-dictionary dicts)
       ;; The dir-locals buffer is now current...
       (let ((dir (file-name-directory buffer-file-name)))
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (and jit-spell-mode
                  buffer-file-name
                  (file-in-directory-p buffer-file-name dir)
                  (funcall doit)))))))))

(define-obsolete-function-alias
  'jit-spell-change-dictionary 'jit-spell-dictionary "0.6")

;;; Minor mode definition

(defun jit-spell--context-menu (menu click)
  "Context menu for `jit-spell-mode'.
MENU and CLICK are as expected of a member of `context-menu-functions'.
It can also be bound to a mouse click to pop up the menu."
  (interactive "i\ne" ignore)
  (save-excursion
    (mouse-set-point click)
    (when-let* ((ov (jit-spell--overlay-at (point)))
                (word (buffer-substring-no-properties
                       (overlay-start ov) (overlay-end ov)))
                (map (or menu (make-sparse-keymap))))
      (dolist (sug (jit-spell--get-suggestions ov))
        (easy-menu-add-item map '("Correct Word")
                            (vector sug (lambda () (interactive)
                                          (jit-spell--apply-correction ov sug)))))
      (easy-menu-add-item map nil `["Save to Dictionary"
                                    (jit-spell--accept-word ,word 'dictionary)])
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

(defvar-keymap jit-spell--dict-modeline-map
  "<mode-line> <mouse-1>" #'jit-spell-dictionary)

(defvar-local jit-spell--dict-modeline nil
  "Memoization place for the function of same name.'")

(defun jit-spell--dict-modeline ()
  (with-memoization jit-spell--dict-modeline
    (when-let* ((dicts (jit-spell--dicts)))
      `((:propertize "/")
        (:propertize ,(mapconcat (lambda (s)
                                   (replace-regexp-in-string "_.*" "" s))
                                 dicts ",")
                     keymap ,jit-spell--dict-modeline-map
                     help-echo ,(format "Current dictionaries: %s"
                                        (string-join dicts ", "))
	             mouse-face mode-line-highlight)))))

(cl-defmethod jit-spell--drop-buffer :after (buffer)
  (with-current-buffer buffer
    (setq jit-spell--dict-modeline nil)))

;;;###autoload
(define-minor-mode jit-spell-mode
  "Just-in-time spell checking."
  :keymap (make-sparse-keymap)
  :lighter (" Spell" (:eval (jit-spell--dict-modeline)))
  (cond
   (jit-spell-mode
    ;; Major mode support
    (cond
     ((derived-mode-p 'prog-mode)
      (add-function :before-until (local 'jit-spell--ignored-p)
                    (lambda (start)
                      (not (jit-spell--has-face-p jit-spell-prog-mode-faces
                                                  (get-char-property start 'face)))))
      (add-function :filter-return (local 'jit-spell--filter-region)
                    (jit-spell--refine-by-face jit-spell-prog-mode-faces t)))
     ((derived-mode-p 'tex-mode 'TeX-mode 'context-mode)
      (add-function :before-until (local 'jit-spell--ignored-p)
                    (lambda (start)
                      (jit-spell--has-face-p jit-spell-tex-ignored-faces
                                             (get-char-property start 'face))))))
    ;; Generic ignore predicate
    (when-let* ((pred (or (bound-and-true-p flyspell-generic-check-word-predicate)
                          (get major-mode 'flyspell-mode-predicate))))
      (add-function :after-until (local 'jit-spell--ignored-p)
                    (lambda (_start) (not (funcall pred)))))
    ;; Buffer setup
    (jit-spell--read-local-words)
    (add-hook 'context-menu-functions 'jit-spell--context-menu nil t)
    (add-hook 'post-command-hook #'jit-spell--unhide-overlay-maybe nil t)
    ;; Font-lock needs to run first
    (add-hook 'jit-lock-functions #'jit-spell--check-region 10 t)
    (jit-lock-register #'jit-spell--check-region))
   (t
    (jit-lock-unregister #'jit-spell--check-region)
    (remove-hook 'post-command-hook #'jit-spell--unhide-overlay-maybe t)
    (remove-hook 'context-menu-functions 'jit-spell--context-menu t)
    (kill-local-variable 'jit-spell--local-words)
    (kill-local-variable 'jit-spell--filter-region)
    (kill-local-variable 'jit-spell--ignored-p)
    (jit-spell--unfontify nil nil t))))

(provide 'jit-spell)

; LocalWords:  jit ispell
;;; jit-spell.el ends here
