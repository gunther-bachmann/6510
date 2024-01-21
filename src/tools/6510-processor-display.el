;; emacs code to display processor status

;; window configuration
(setopt
 display-buffer-alist
 '(("^\\* 6510 processor \\*$"
    (display-buffer-below-selected display-buffer-at-bottom)
    (inhibit-same-window . t)
    (window-height . 6))))

(defconst 6510-proc-buffer-name "* 6510 processor *")

(defun 6510-debugger--generate-change-str (org-str str)
  (if (or (string-equal str org-str)
         (string-empty-p org-str)
         (string-empty-p str))
      str
    (let ((new-str "")
          (str-index 0)
          (red-start-index -1))
      (--each (string-to-list str)
        (if (char-equal it (string-to-char (substring org-str it-index (1+ it-index))))
            (when (and (>= red-start-index 0)
                     (not (string-match-p "[0-9a-zA-Z$#_]" (char-to-string it))))
              (progn
                (setq new-str
                      (string-join (list new-str
                                         (propertize (substring str red-start-index it-index) 'font-lock-face 'diff-error))))
                (setq red-start-index -1)
                (setq str-index it-index)))
          (when (and (< red-start-index 0)
                     (< str-index it-index))
            (setq new-str (string-join (list new-str (substring str str-index it-index))))
            (setq str-index -1)
            (setq red-start-index it-index))))
      (if (>= str-index 0)
          (setq new-str (string-join (list new-str (substring str str-index))))
        (setq new-str
              (string-join (list new-str
                                 (propertize (substring str red-start-index) 'font-lock-face 'diff-error)))))
      new-str)))

(defun 6510-proc-buffer-display (str)
  (save-current-buffer
    (let* ((proc-buff (get-buffer-create 6510-proc-buffer-name))
           (proc-win  (get-buffer-window proc-buff)))
      (unless proc-win
        (with-selected-window (selected-window) ;; prevents switch to new window
          (pop-to-buffer proc-buff)
          (set-window-dedicated-p (get-buffer-window proc-buff) t)
          (font-lock-mode 1)))
      (with-current-buffer proc-buff
        (let ((buff-size (point-max))
              (new-size (length str)))
          (read-only-mode 0)
          (goto-char (point-min))
          (overwrite-mode 1)
          (insert (6510-debugger--generate-change-str (substring-no-properties (buffer-string)) str))
          (delete-region (point) (point-max))
          (set-buffer-modified-p nil)
          (read-only-mode 1))))))

(defun 6510-proc-buffer-kill ()
  (ignore-errors
    (kill-buffer 6510-proc-buffer-name)))
