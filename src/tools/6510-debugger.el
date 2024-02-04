;; elisp code to execute debugger functions in the current source window,
;; sending these commands to a running racket debugger process

(defun 6510-debugger--execute-command-in-repl (command-str &optional submit)
  (with-current-buffer (racket-repl-buffer-name-shared)
    (let ((quoted-command (replace-regexp-in-string "\"" "\\\\\"" command-str)))
      (when submit
        (racket--repl-add-to-input-history command-str))
      (insert command-str)
      (insert "\n")
      (racket--repl-delete-prompt-mark nil)
      (set-marker racket--repl-output-mark (point))
      (let ((send-type (if submit "repl-submit" "repl-input")))
        (process-send-string
         (get-process (racket--back-end-process-name))
         (format "(%s %s %s \"%s\n\")"
                 racket--cmd-nonce
                 (racket--repl-session-id)
                 send-type
                 quoted-command))))))

;; (6510-debugger--execute-command-in-repl "(run-debugger 2064 raw-bytes \"6510-example.rkt\")" t)
;; (6510-debugger--execute-command-in-repl "s")
;; (6510-debugger--execute-command-in-repl "q")

(defun 6510-debugger-execute-startup ()
  "execute debugger startup"
  (interactive)
  (6510-debugger--execute-command-in-repl "(run-debugger 2064 raw-bytes \"6510-example.rkt\")" t))

(defun 6510-debugger-step ()
  "execute one debugger step"
  (interactive)
  (6510-debugger--execute-command-in-repl "step"))

(defun 6510-debugger-back ()
  "execute one debugger step back"
  (interactive)
  (6510-debugger--execute-command-in-repl "back"))

(defun 6510-debugger-run ()
  "run until breakpoint or break is encountered"
  (interactive)
  (6510-debugger--execute-command-in-repl "run"))

(defun 6510-debugger-quit ()
  "quit debugger session"
  (interactive)
  (6510-debugger--execute-command-in-repl "q"))

(defun hex-number-p (hex-num)
  (when (and hex-num (stringp hex-num))
    (string-match-p "^[a-fA-F0-9]+$" hex-num)))

(hex-number-p "081a")

(defun 6510-debugger-toggle-break-point ()
  "toggle a break point at the current line"
  (interactive)
  (save-excursion
    (when-let ((hex-num-ov
                (--find (and (overlay-get it 'before-string)
                           (hex-number-p (string-trim (substring-no-properties (overlay-get it 'before-string)))))
                        (overlays-at (line-beginning-position))))
               (hex-location (overlay-get hex-num-ov 'before-string))
               (hex-str (string-trim (substring-no-properties hex-location))))
      (message "hexlocation %s" hex-location)
      (if (6510-debugger--visualized-break-point-p)
          (progn
           (6510-debugger--remove-visualized-break-point)
           (6510-debugger--execute-command-in-repl (format "clear stop pc = %s" hex-str)))
        (progn
         (6510-debugger--visualize-break-point)
         (6510-debugger--execute-command-in-repl (format "stop pc = %s" hex-str)))))))

(defun 6510-debugger--visualize-break-point ()
  (let ((before-string (propertize " " 'display '(left-fringe large-circle)))
        (new-overlay (make-overlay (line-beginning-position) (1+ (line-beginning-position)))))
    (overlay-put new-overlay 'before-string before-string)
    (overlay-put new-overlay 'category "breakpoint")))

(defun 6510-debugger--remove-visualized-break-point ()
  (--each (--filter (equal "breakpoint" (overlay-get it 'category))
                    (overlays-in (line-beginning-position) (line-end-position)))
    (delete-overlay it)))

(defun 6510-debugger--visualized-break-point-p ()
  (--find (equal "breakpoint" (overlay-get it 'category))
          (overlays-in (line-beginning-position) (line-end-position))))


(require 'racket-xp)
(defvar 6510-debugger-mode-map
  (define-keymap :parent racket-xp-mode-map
    "C-c d x" #'6510-debugger-execute-startup
    "C-c d r" #'6510-debugger-run
    "C-c d n" #'6510-debugger-step
    "C-c d p" #'6510-debugger-back
    "C-c d b" #'6510-debugger-toggle-break-point
    "C-c d q" #'6510-debugger-quit))

(define-minor-mode 6510-connected-debugger-mode
  "connect the given source code with a racket debugger process and execute commands"
  :keymap 6510-debugger-mode-map
  (if 6510-connected-debugger-mode
      (progn
        (message "enable 6510 connected debugger mode")
        (read-only-mode 1))
    (message "disable 6510 connected debugger mode")
    (read-only-mode 0)))

(provide '6510-debugger)
