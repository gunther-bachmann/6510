;; elisp code to work with 6510 debugger

(defun 6510-debugger--overlay-source (file-name line)
  "highlight line in the buffer visiting the given file"
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (unless hl-line-mode (hl-line-mode 1))
      (goto-line line)
      (hl-line-unhighlight)
      (hl-line-highlight)
      (when-let ((sel-win (get-buffer-window buffer)))
        (with-selected-window sel-win
          (goto-line line)
          (recenter nil t))))))

(defun 6510-debugger--remove-overlay-source (file-name)
  "highlight line in the buffer visiting the given file"
  (save-excursion
    (when-let ((buffer (or (find-buffer-visiting (format "./%s" file-name)))))
      (with-current-buffer buffer
        (unless hl-line-mode (hl-line-mode 1))
        (hl-line-unhighlight)))))
