;; elisp code to work with the racket tools for the 6510 processor

(require '6510-debugger-sync-source)
(require '6510-processor-display)
(require '6510-debugger)
(require '6510-screen-display)
(require 'nmil-debugger-stack-display)

;;;###autoload
(defun 6510-debugger--has-proc-display-cap () t)
;;;###autoload
(defun 6510-debugger--has-single-step-cap (file-name)
  "allow line selection for single step, if file exists"
  t)
;;;###autoload
(defun 6510-debugger--has-assembly-disp-cap (file-name)
  "can only display complete assembly in file if it is in connected debugger mode"
  (let ((vbuf (find-buffer-visiting file-name)))
    (if vbuf
        (with-current-buffer vbuf
          6510-connected-debugger-mode)
      nil)))
;;;###autoload
(defun 6510-debugger--has-output-cap () t)
;;;###autoload
(defun nmil-debugger--has-stack-display-cap () t)

(provide '6510-emacs-integration)
