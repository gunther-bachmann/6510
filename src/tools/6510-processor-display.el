;; emacs code to display processor status

;; window configuration
;; (setopt
;;  display-buffer-alist
;;  '(("^\\* 6510 processor \\*$"
;;     (display-buffer-below-selected display-buffer-at-bottom)
;;     (inhibit-same-window . t)
;;     (window-height . 4))))

(defconst 6510-proc-buffer-name "* 6510 processor *")

(defun 6510-proc-buffer-display (str)
  (save-current-buffer
    (let* ((proc-buff (get-buffer-create 6510-proc-buffer-name))
           (proc-win  (get-buffer-window proc-buff)))
      (unless proc-win
        (with-selected-window (selected-window) ;; prevents switch to new window
          (pop-to-buffer proc-buff)
          (set-window-dedicated-p (get-buffer-window proc-buff) t)))
      (with-current-buffer proc-buff
        (let ((buff-size (point-max))
              (new-size (length str)))
          (read-only-mode 0)
          (goto-char (point-min))
          (overwrite-mode 1)
          (insert str)
          (delete-region (point) (point-max))
          (set-buffer-modified-p nil)
          (read-only-mode 1))))))

(defun 6510-proc-buffer-kill ()
  (ignore-errors
    (kill-buffer 6510-proc-buffer-name)))
