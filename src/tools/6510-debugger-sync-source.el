;; elisp code to work with 6510 debugger

(defun 6510-debugger--overlay-source-line (file-name line pc-str)
  "add overlay of pc to line of file"
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (unless hl-line-mode (hl-line-mode 1))
      (goto-line line)
      (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
        (ov-set ov 'before-string (propertize pc-str 'font-lock-face 'font-lock-doc-face))))))

(defun 6510-debugger--remove-overlay-source-lines (file-name)
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (--each (overlays-in (point-min) (point-max))
        (delete-overlay it)))))

(defconst 6510-debugger--command-overlay-column 60)

(defun 6510-debugger--remove-old-disassembly-overlays ()
  (--each
      (overlays-in (point-min) (point-max))
    (when (overlay-get it 'disassembly)
      (delete-overlay it))))

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
                                (+ (line-beginning-position) column)))))
    (ov-set ov 'after-string (propertize (format "%s%s" prefix str)
                                         'font-lock-face 'font-lock-doc-face))
    (ov-set ov 'disassembly t)))

(defun 6510-debugger--overlay-source (file-name line disassembled-str)
  "highlight line in the buffer visiting the given file"
  (when-let ((buffer (find-buffer-visiting (format "./%s" file-name))))
    (with-current-buffer buffer
      (6510-debugger--remove-old-disassembly-overlays)
      (unless hl-line-mode (hl-line-mode 1))
      (goto-line line)
      (hl-line-unhighlight)
      (hl-line-highlight)
      (let ((line-len (- (line-end-position) (line-beginning-position))))
        (when (> line-len 6510-debugger--command-overlay-column)
          (6510-debugger--hide-tail-of-long-line
           6510-debugger--command-overlay-column))
        (6510-debugger--show-tail-string-overlay
         6510-debugger--command-overlay-column
         disassembled-str))
      (when-let ((sel-win (get-buffer-window buffer)))
        (with-selected-window sel-win
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter nil t))))))

(defun 6510-debugger--remove-overlay-source (file-name)
  "highlight line in the buffer visiting the given file"
  (save-excursion
    (when-let ((buffer (or (find-buffer-visiting (format "./%s" file-name)))))
      (with-current-buffer buffer
        (unless hl-line-mode (hl-line-mode 1))
        (hl-line-unhighlight)))))
