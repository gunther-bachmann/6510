;; emacs code to display processor status

;; window configuration


(defconst nmil-stack-buffer-name "* nmil stack *")

;;;###autoload
(defun nmil-debugger--stack-buffer-display (str)
  (save-current-buffer
    (let* ((stack-buff (get-buffer-create nmil-stack-buffer-name))
           (stack-win  (get-buffer-window stack-buff)))
      (unless stack-win
        (with-selected-window (selected-window) ;; prevents switch to new window
          (pop-to-buffer stack-buff)
          (set-window-dedicated-p (get-buffer-window stack-buff) t)
          (font-lock-mode 1)))
      (with-current-buffer stack-buff
        (let ((buff-size (point-max))
              (new-size (length str)))
          (read-only-mode 0)
          (goto-char (point-min))
          (overwrite-mode 1)
          (insert str)
          (delete-region (point) (point-max))
          (set-buffer-modified-p nil)
          (read-only-mode 1))))))

;;;###autoload
(defun nmil-debugger--stack-buffer-kill ()
  (ignore-errors
    (kill-buffer nmil-stack-buffer-name)))

(provide 'nmil-debugger-stack-display)
