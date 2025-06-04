;; This file contains code to allow for automatic document generation using tags (see src/nmil/vm-documentation.org):
;;  @DC-FUN
;;  @DC-ZP
;;  @DC-C
;;  @DC-M


(defun gb/racket--rgdc--enrich (dc-plists)
  "enrich all plists in DC-PLISTS collected by rip grep"
  (mapcar
   #'gb/racket--rgcd--enrich-plist-fun
   dc-plists))

(defun gb/racket--rgcd--surrounding-define (filename line)
  "get the define by searching backward from the given LINE in the FILENAME"
  (save-excursion
    (when (file-exists-p filename)
      (with-current-buffer (find-file filename)
        (goto-line (string-to-number line))
        (end-of-line)
        (search-backward-regexp "define *\\([A-Z_]*\\)")
        (substring-no-properties (match-string 1))))))

(defun gb/racket--rgcd--next-doclines (filename line)
  "get the next doclines until a define follows starting from LINE in FILENAME"
  (save-current-buffer
    (when (file-exists-p filename)
      (with-temp-buffer
        (find-file filename)
        (goto-line (+ 1 (string-to-number line)))
        (beginning-of-line)
        (let ((beg (point)))
          (search-forward-regexp "define *\\([A-Z_]*\\)")
          (beginning-of-line)
          (buffer-substring-no-properties beg (- (point) 1)))))))

(defun gb/racket--rgcd--next-define (filename line)
  "get the next define starting the search from LINE in FILENAME"
  (save-current-buffer
    (when (file-exists-p filename)
      (with-temp-buffer
        (find-file filename)
        (goto-line (string-to-number line))
        (end-of-line)
        (search-forward-regexp "(define *\\([A-Za-z0-9-_]*\\)")
        (substring-no-properties (match-string 1))))))

(defun gb/racket--rgcd--cleanup-doc-lines (doc-lines)
  (--filter (and (not (string-prefix-p "@D" it))
               (not (string-prefix-p "---" it)))
            (mapcar (lambda (doc-line)
                      (string-remove-prefix
                       " "
                       (string-remove-prefix
                        ";"
                        (string-remove-prefix
                         ";"
                         doc-line))))
                    (-take-while
                     (lambda (line) (and (string-prefix-p ";" line)
                                  (not (string-match-p "^;+ *@DC-" line))))
                     doc-lines))))

;; (gb/racket--rgcd--cleanup-doc-lines
;;  (list ";; line one"
;;        "; line two"
;;        ";;@DC-C other"   ;; ignore this line, don't take anything here after
;;        ";line three"
;;        ";;line four"
;;        "(define "       ;; don't take from hereon
;;        ";; line five"
;;        ";;@DC-C some"))

(defun gb/racket--rgcd--enrich-plist-fun (plist)
  "enrich the given PLIST with function data (if it is of type FUN)"
  (if (not (string= "FUN" (plist-get plist :type)))
      plist
    (let ((next-define-label
           (or
            (gb/racket--rgcd--next-define
             (plist-get plist :filename)
             (plist-get plist :line))
            "UNKNOWN")))
      `( :doclines ,(gb/racket--rgcd--cleanup-doc-lines
                     (split-string (gb/racket--rgcd--next-doclines
                                    (plist-get plist :filename)
                                    (plist-get plist :line))
                                   "\n"))
         :racket-require ,(format "(require (only-in \"%s\" %s))" (plist-get plist :filename) next-define-label)
         :assembler-include ,next-define-label
         :org-link ,(format "file:%s::%s" (plist-get plist :filename) (plist-get plist :line))
         ,@plist))))

;; (gb/racket--rgcd--enrich-plist-fun (gb/racket--rgdc-line-to-plist "repo/+1/6510/src/nmil/vm-mm-pages.rkt:176:4:;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages"))

(defconst gb/racket--rgcd--org-template
  (list "#+title: vm documentation"
        "* constants / tag bytes"
        "* zero-page-locations (by address)"
        "* zero-page-locations (by name)"
        "* zero-page-locations (by group)"
        "* memory-locations (by address)"
        "* memory-locations (by name)"
        "* memory-locations (by group)"
        "* functions (by name)"
        "* functions (by group)"
        "* byte code (by code)"
        "* byte code (by name)"
        "* byte code (by group)"
        "* - :noexport:"
        "#+begin_src emacs-lisp"
        "  ;; Local Variables:"
        "  ;; org-pretty-entities-include-sub-superscripts: nil"
        "  ;; End:"
        "#+end_src"
        ))

(defun gb/racket--rgcd--org-file-create (org-file)
  (with-temp-buffer
    (mapc #'(lambda (line) (insert (format "%s\n" line))) gb/racket--rgcd--org-template)
    (write-file org-file nil)))

;; (gb/racket--rgcd--org-file-create "some.org")

(defun gb/racket--rgcd--org-fun->string (level plist)
  (or
   (ignore-errors
     (let* ((file-ref (plist-get plist :org-link))
            (fun-label (plist-get plist :label))
            (fun-label-docline-sep (make-string (min 0 (- 40 (length fun-label))) ?\ ))
            (fun-doc-line (car (or (plist-get plist :doclines) (list "no doc"))))
            (racket-require (plist-get plist :racket-require))
            (assembler-include (plist-get plist :assembler-include))
            (indented-doc-lines (string-join (plist-get plist :doclines) "\n  ")))
       (format "- [[%s][%s]] :: %s %s\n  - racket require :: %s\n  - assembler include :: %s\n  %s"
               file-ref
               fun-label
               fun-label-docline-sep
               fun-doc-line
               racket-require
               assembler-include
               indented-doc-lines)))
   (format "- ERROR ON PROCESSING %S" plist)))

(defun gb/racket--rgcd--loop-over-lists-and-sort ()
  (save-excursion
    (goto-char 0)
    (search-forward-regexp "^- " nil t)
    (while (search-forward-regexp "^- " nil t)
      (org-sort-list nil ?a nil nil nil)
      (search-forward-regexp "^\\* " nil t))
    (save-buffer)))

(defun gb/racket--rgcd--jump-to-group (groups level max-point)
  "search forward to find the given group but don't leave the current heading!"
  (when groups
    (let ((group-entry (format "%s %s" (make-string level ?\*) (car groups))))
      (message (format "group-entry: %s" group-entry))
      (unless (search-forward group-entry max-point t)
        (insert (format "\n%s %s" (make-string level ?\*) (car groups))))
      (gb/racket--rgcd--jump-to-group (cdr groups) (+ 1 level) max-point))))

(defun gb/racket--rgcd--gen-org-fun (plist org-file)
  "generate all entries for function described by PLIST into ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgcd--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp "^* functions \(by name\)")
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-fun->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp "^* functions \(by group\)")
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgcd--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgcd--org-fun->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgcd ()
  "execute documentation generation within this folder (and subfolders)"
  (interactive)
  (let ((file-name (read-string "org file name to generate: " "vm-documentation.test.org")))
    (when (file-exists-p file-name)
      (delete-file file-name nil))
    (mapc (lambda (plist) (gb/racket--rgcd--gen-org-fun plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "FUN")))
    (with-temp-buffer (find-file file-name)
                      (gb/racket--rgcd--loop-over-lists-and-sort))))

(defun gb/racket--rip-grep-dc (suffix)
  "return list of p-lists for SUFFIX by rip grepping the current folder"
  (mapcar
   #'gb/racket--rgdc-line-to-plist
   (remove ""
         (split-string
          (string-trim (shell-command-to-string (format "rg -e '@DC-%s' -t racket --vimgrep" suffix)))
          "\n"))))

;; (gb/racket--rgdc--enrich
;;  (gb/racket--rip-grep-dc "FUN"))

;; (split-string
;;     (string-trim (shell-command-to-string (format "rg -e '@DC-%s' -t racket --vimgrep" "X")))
;;     "\n")

(defun gb/racket--rgdc-line-to-plist (line)
  "get a plist for the given rip grep LINE"
  (let* ((tokens (split-string line ":"))
         (match  (string-join (cdddr tokens) ":")))
    `(:filename ,(nth 0 tokens)
                :line     ,(nth 1 tokens)
                :column   ,(nth 2 tokens)
                ,@(gb/racket--rgdc-match--label match))))

;; (gb/racket--rgdc-line-to-plist "src/nmil/vm-mm-pages.rkt:176:4:;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages")

;; (plist-get (gb/racket--rgdc-line-to-plist "src/nmil/vm-mm-pages.rkt:176:4:;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages") :filename)

(defun gb/racket--rgdc-match--label (match-line)
  "convert the MATCH-LINE into a plist"
  (string-match "^;; *@DC-\\([^:]*\\): *\\([^ ,]+\\)\\(, *group: *\\([^ ,]*\\)\\)?" match-line)
  `(:type  ,(match-string 1 match-line)
    :label ,(match-string 2 match-line)
    :group ,(or (when (match-string 4 match-line) (split-string (match-string 4 match-line) "-")) (list))))

;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages-sub-a")
;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages-sub-a")
;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages")
;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER")
