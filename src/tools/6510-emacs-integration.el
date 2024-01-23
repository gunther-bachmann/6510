;; elisp code to work with the racket tools for the 6510 processor

(require '6510-debugger-sync-source)
(require '6510-processor-display)

;;;###autoload
(defun 6510-debugger--has-proc-display-cap () t)
;;;###autoload
(defun 6510-debugger--has-single-step-cap (file-name)
  (if (find-buffer-visiting file-name)
      t
    nil))

(provide '6510-emacs-integration)
