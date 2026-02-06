;; elisp code to work with 6510 debugger

;;;###autoload
(defun 6510-debugger--show-address-on-source-line (file-name line pc-str)
  "add overlay of pc to line of file"
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (let ((ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)))))
          (ov-set ov 'before-string (propertize pc-str 'font-lock-face '(:foreground "orange4" :slant normal))))))))

;;;###autoload
(defun 6510-debugger--remove-all-overlay (file-name)
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (--each (overlays-in (point-min) (point-max))
        (delete-overlay it)))))

(defconst 6510-debugger--command-overlay-column 55)

;;;###autoload
(defun 6510-debugger--remove-disassembly-on-source-lines (file-name)
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (--each
          (overlays-in (point-min) (point-max))
        (when (overlay-get it 'disassembly)
          (delete-overlay it))))))

(defun 6510-debugger--hide-tail-of-long-line (column)
  (let ((ov-hide (make-overlay (+ (line-beginning-position) (1+ column))
                               (line-end-position))))
    (ov-set ov-hide 'invisible t)
    (ov-set ov-hide 'disassembly t)))

(defun 6510-debugger--show-tail-string-overlay (column str)
  (let* ((line-len (- (line-end-position) (line-beginning-position)))
         (prefix (make-string (max 1 (- (1+ column) line-len)) ?\ ))
         (ov (make-overlay (line-beginning-position)
                           (min (line-end-position)
                                (+ (line-beginning-position) column))))
         (strs (split-string str ":"))
         (adr (car strs))
         (dis (cadr strs)))
    (ov-set ov 'after-string
            (string-join
             (list (propertize (format "%s%s:" prefix adr)
                               'font-lock-face '(:foreground "orange4" :slant normal))
                   (propertize (format "%s " dis)
                               'font-lock-face '(:foreground "orange3" :slant normal)))
             ""))
    (ov-set ov 'disassembly t)))

;;;###autoload
(defun 6510-debugger--show-disassembly-on-source-lines (file-name line disassembled-str)
  "highlight line in the buffer visiting the given file"
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (let ((orig-point (point)))
        (goto-char (point-min))
        (forward-line (1- line))
        (unless (ov-in 'disassembly t (line-beginning-position) (line-end-position))
          (setq orig-point nil))
        (6510-debugger--remove-disassembly-on-source-lines file-name)
        (6510-debugger--show-tail-string-overlay
         6510-debugger--command-overlay-column
         disassembled-str)
        (if orig-point
            (goto-char orig-point)
          (run-hooks 'post-command-hook))))))

;;;###autoload
(defun 6510-debugger--move-cursor-to-source-line (file-name line)
  (when (and (not (and follow-source-window (window-valid-p follow-source-window)))
           (buffer-file-name (window-buffer (selected-window))))
    ;; (message (format "follow-source-window set to %S" follow-source-window))
    (setq follow-source-window (selected-window)))
  (when (and follow-source-window (not buffer-file-name))
    ;; (message "follow-source-window and no buffer file name (I am in repl)")
    (with-selected-window follow-source-window
      (when-let ((buffer (find-file (if (string-prefix-p "/" file-name) file-name (format "./%s" file-name)))))
        (when-let ((sel-win (get-buffer-window buffer)))
          (with-selected-window sel-win
            (hl-line-mode 1) ;; ensure it is on
            (goto-char (point-min))
            (forward-line (1- line))
            (hl-line-highlight)
            (recenter nil t)))))))

(defvar follow-source-window nil "the window to be used to follow source references")

;; (defun 6510-debugger--remove-highlighted-execution-line (file-name)
;;   "highlight line in the buffer visiting the given file"
;;   (save-excursion
;;     (when-let ((buffer (or (find-buffer-visiting (format "./%s" file-name)))))
;;       (with-current-buffer buffer
;;         (unless hl-line-mode (hl-line-mode 1))
;;         (hl-line-unhighlight)))))

(provide '6510-debugger-sync-source)
