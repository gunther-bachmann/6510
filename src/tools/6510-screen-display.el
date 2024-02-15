;; utility functions to display out put in a separate windw

(defconst 6510-debugger-output-buffer "* 6510 output *")

(defvar output-running nil)

(defun 6510-debugger--print-string (str)
  (when output-running
    (sit-for 1))
  (setq output-running t)
  (6510-debugger--ensure-output-window)
  (with-selected-window (get-buffer-window 6510-debugger-output-buffer)
    (goto-char (point-max))
    (insert str))
  (setq output-running nil))

(defun 6510-debugger--ensure-output-window ()
  (unless (and (get-buffer-window 6510-debugger-output-buffer)
             (memq (get-buffer-window 6510-debugger-output-buffer) (window-list)))
    (split-window-below -20)
    (other-window 1)
    (let ((buffer (or (get-buffer 6510-debugger-output-buffer) (generate-new-buffer 6510-debugger-output-buffer))))
      (set-window-buffer (selected-window) buffer)
      (set-buffer-major-mode buffer))
    (other-window -1)))

(provide '6510-screen-display)
