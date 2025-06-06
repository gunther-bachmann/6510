;; This file contains code to allow for automatic document generation using tags (see src/nmil/vm-documentation.org):
;;  @DC-FUN
;;  @DC-ZP
;;  @DC-C
;;  @DC-M

(defconst gb/racket--rgdc--define-regex
  "define *\\([A-Za-z0-9-_]*\\)"
  "regular expression with which to search for racket define")


(defun gb/racket--rgdc--enrich (dc-plists)
  "enrich all plists in DC-PLISTS collected by rip grep"
  (mapcar
   #'gb/racket--rgcd--enrich-plist-c
   (mapcar
    #'gb/racket--rgcd--enrich-plist-ml
    (mapcar
     #'gb/racket--rgcd--enrich-plist-zp
     (mapcar
      #'gb/racket--rgcd--enrich-plist-fun
      dc-plists)))))

(defun gb/racket--rgcd--surrounding-define (filename line)
  "get the define by searching backward from the given LINE in the FILENAME"
  (save-excursion
    (when (file-exists-p filename)
      (with-current-buffer (find-file filename)
        (goto-line (string-to-number line))
        (end-of-line)
        (search-backward-regexp gb/racket--rgdc--define-regex)
        (substring-no-properties (match-string 1))))))

(defun gb/racket--rgcd--zp-doclines (filename line)
  "get the next doclines until the second word/byte-const follows starting from LINE in FILENAME"
  (save-current-buffer
    (when (file-exists-p filename)
      (with-temp-buffer
        (find-file filename)
        (goto-line (+ 1 (string-to-number line)))
        (beginning-of-line)
        (let ((beg (point)))
          (search-forward-regexp gb/racket--rgdc--byte-word-val-regex)
          (end-of-line)
          (buffer-substring-no-properties beg (- (point) 1)))))))

(defun gb/racket--rgcd--next-doclines (filename line)
  "get the next doclines until a define follows starting from LINE in FILENAME"
  (save-current-buffer
    (when (file-exists-p filename)
      (with-temp-buffer
        (find-file filename)
        (goto-line (+ 1 (string-to-number line)))
        (beginning-of-line)
        (let ((beg (point)))
          (search-forward-regexp gb/racket--rgdc--define-regex)
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
        (search-forward-regexp gb/racket--rgdc--define-regex)
        (substring-no-properties (match-string 1))))))

(defun gb/racket--rgcd--cleanup-doc-lines (doc-lines)
  (--filter (and (not (string-prefix-p "@D" it))
               (not (string-prefix-p "---" it)))
            (mapcar (lambda (doc-line)
                      (string-match "^[^;]*;+ ?" doc-line)
                      (let ((me (match-end 0)))
                        (if me
                            (substring doc-line me)
                          doc-line)))
                    (-take-while
                     (lambda (line) (and (string-match-p ";.+" line)
                                  (not (string-match-p ";+ *@DC-" line))))
                     (-filter (lambda (line) (stringp line))
                              doc-lines)))))

;; (gb/racket--rgcd--cleanup-doc-lines
;;  (list "(some code) ;; line one"
;;        "; line two"
;;        "(line without comment)"
;;        ";;@DC-C other"   ;; ignore this line, don't take anything here after
;;        ";line three"
;;        ";;line four"
;;        "(define "       ;; don't take from hereon
;;        ";; line five"
;;        ";;@DC-C some"))

;; (gb/racket--rgcd--cleanup-doc-lines
;;  (list
;;   "   ;; zero page location 3 for temp usage"
;;   "(byte-const ZP_TEMP3                  $d9) ;; may be used as pointer (in combination with ZP_TEMP4 => must be in adjacent memory locations)"))

(defun gb/racket--rgcd--this-docline (filename line)
  "read the doc within the line"
  "UNKNOWN DOC")

(defconst gb/racket--rgdc--byte-word-val-regex "\\(byte\\|word\\)-const *[A-Za-z0-9_-]+ *\\(\\$[a-fA-F0-9]+\\)")
(defun gb/racket--rgcd--following-address (filename line)
  "get the address/value of the following byte-const or word-const definition"
  (ignore-errors
    (save-current-buffer
      (when (file-exists-p filename)
        (with-temp-buffer
          (find-file filename)
          (goto-line (string-to-number line))
          (end-of-line)
          (search-forward-regexp gb/racket--rgdc--byte-word-val-regex nil t)
          (list
           (substring-no-properties (match-string 2))
           (substring-no-properties (match-string 1))))))))

(defun gb/racket--rgcd--enrich-plist-ml (plist)
  (if (not (string= "M" (plist-get plist :type)))
      plist
    (gb/racket--rgcd--enrich-plist-assconst plist)))

(defun gb/racket--rgcd--enrich-plist-c (plist)
  (if (not (string= "C" (plist-get plist :type)))
      plist
    (gb/racket--rgcd--enrich-plist-constant plist)))

(defun gb/racket--rgcd--enrich-plist-zp (plist)
  "enrich the given PLIST with zero page data (if it is of type ZP)"
  (if (not (string= "ZP" (plist-get plist :type)))
      plist
    (gb/racket--rgcd--enrich-plist-constant plist)))

(defconst gb/racket--rgdc--byte-word-const-regex "\\(byte\\|word\\) +\\$[a-fA-F0-9]+")
(defun gb/racket--rgcd--following-len (filename line)
  "return the length of this assembler constant (either byte or word)"
  (ignore-errors
    (save-current-buffer
      (when (file-exists-p filename)
        (with-temp-buffer
          (find-file filename)
          (goto-line (string-to-number line))
          (end-of-line)
          (search-forward-regexp gb/racket--rgdc--byte-word-const-regex nil t)
          (substring-no-properties (match-string 1)))))))


(defconst gb/racket--rgdc--word-const-regex "word +\\$[a-fA-F0-9]+")
(defconst gb/racket--rgdc--byte-const-regex "byte +\\$[a-fA-F0-9]+")
(defconst gb/racket--rgdc--org-command "(org #x\\([a-fA-F0-9]+\\))")
(defun gb/racket--rgcd--find-address-by-org (filename line)
  "find org, count bytes and words in between to get to address"
  (ignore-errors
    (save-current-buffer
      (when (file-exists-p filename)
        (with-temp-buffer
          (find-file filename)
          (goto-line (string-to-number line))
          (end-of-line)
          (let ((max-point (point)))
            (search-backward-regexp gb/racket--rgdc--define-regex nil t)
            (let ((min-point (point)))
              (search-forward-regexp gb/racket--rgdc--org-command nil t)
              (let ((org-num (match-string 1)))
                (let ((bytes-num (count-matches gb/racket--rgdc--byte-const-regex min-point max-point nil))
                      (words-num (count-matches gb/racket--rgdc--word-const-regex min-point max-point nil)))
                  (format "$%x" (+ (string-to-number org-num 16) bytes-num (* 2 words-num))))))))))))

(defun gb/racket--rgcd--assconst-doclines (filename line)
  "find doc lines until next label | DC"
  (save-current-buffer
    (when (file-exists-p filename)
      (with-temp-buffer
        (find-file filename)
        (goto-line (+ 1 (string-to-number line)))
        (beginning-of-line)
        (let ((beg (point)))
          (search-forward-regexp gb/racket--rgdc--byte-word-const-regex)
          (end-of-line)
          (buffer-substring-no-properties beg (- (point) 1)))))))

(defun gb/racket--rgcd--enrich-plist-assconst (plist)
  (let ((surrouding-define-label (or
                                  (gb/racket--rgcd--surrounding-define
                                   (plist-get plist :filename)
                                   (plist-get plist :line))
                                  "UNKNOWN"))
        (address (or (gb/racket--rgcd--find-address-by-org
                     (plist-get plist :filename)
                     (plist-get plist :line))
                    "UNKNOWN"))
        (len (or (gb/racket--rgcd--following-len
                 (plist-get plist :filename)
                 (plist-get plist :line))
                "UNKNOWN")))
    (message (format "found address %s" address))
    `( :doclines ,(gb/racket--rgcd--cleanup-doc-lines
                   (split-string (gb/racket--rgcd--assconst-doclines
                                  (plist-get plist :filename)
                                  (plist-get plist :line))
                                 "\n"))
       :address ,address
       :length ,len
       :racket-require ,(format "(require (only-in \"%s\" %s))" (plist-get plist :filename) surrouding-define-label)
       :assembler-include ,surrouding-define-label
       :org-link ,(format "file:%s::%s" (plist-get plist :filename) (plist-get plist :line))
       ,@plist)))

(defun gb/racket--rgcd--enrich-plist-constant (plist)
  (let ((surrouding-define-label
         (or
          (gb/racket--rgcd--surrounding-define
           (plist-get plist :filename)
           (plist-get plist :line))
          "UNKNOWN"))
        (address-n-len (or (gb/racket--rgcd--following-address
                           (plist-get plist :filename)
                           (plist-get plist :line))
                          (list "UNKNOWN" "UNKNOWN"))))
    `( :doclines ,(gb/racket--rgcd--cleanup-doc-lines
                   (split-string (gb/racket--rgcd--zp-doclines
                                  (plist-get plist :filename)
                                  (plist-get plist :line))
                                 "\n"))
       :address ,(car address-n-len)
       :length ,(cadr address-n-len)
       :racket-require ,(format "(require (only-in \"%s\" %s))" (plist-get plist :filename) surrouding-define-label)
       :assembler-include ,surrouding-define-label
       :org-link ,(format "file:%s::%s" (plist-get plist :filename) (plist-get plist :line))
       ,@plist)))

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
        "- collection of all annotated elements"
        "- @DC-FUN :: function annotation"
        "  @DC_FUN: <fun-label>(, group: <fun-group>(-<fun-group)*)*"
        "  example: ;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages"
        "  may have multiple groups"
        "  the file name is used for creating the racket require line"
        "  the define (following) is used for racket require line"
        "  the @DC-FUN line is used for the org-link"
        "  the line following this @DC-FUN comment must:"
        "  - have 1..n comments (describing the function)"
        "  - have a single \"define <some-label>\" (which must include the fun-label in its body)"
        "- @DC-ZP :: zero page annotation"
        "  @DC-ZP: <zero-page-constant>(, group: <zp-group>(-<zp-group>)*)*"
        "  example: ;; @DC-ZP: ZP_RA, group: registers-array"
        "  the file name is used for creating the racket require line"
        "  the enclosing define is used for racket require line"
        "  the @DC-ZP line is used for the org-link"
        "  the line following this @DC-ZP comment must:"
        "  - have a single (byte-const <zero-page-constant>) followed by a describing comment"
        "  - or have a single (word-const <zero-page-constant>) followed by a describing comment"
        "- @DC-C :: constant"
        "  @DC-C: <constant-label>(, group: <c-group>(-<cgroup>)*)*"
        "  example: ;; @DC-C: TAG_BYTE_BYTE_CELL"
        "  the file name is used for creating the racket require line"
        "  the enclosing define is used for racket require line"
        "  the @DC-C line is used for the org-link"
        "  the line following this @DC-C comment must:"
        "  - have a single (byte-const <zero-page-constant>) followed by a describing comment"
        "  - or have a single (word-const <zero-page-constant>) followed by a describing comment"
        "- @DC-M :: memory (locations)"
        "  @DC-M: <mem-label>(, group: <m-group>(-<m-group)*)*"
        "  example: ;; @DC-M: GLOBAL_CELLPAIR_PAGE_FOR_ALLOC, group: gc"
        "  the file name is used for creating the racket require line"
        "  the enclosing define is used for racket require line, must start with an org (<- allows for actual location derivation)"
        "  the @DC-M line is used for the org-link"
        "  the line following this @DC-M comment must:"
        "  - be a line (label <mem-label>) with comments followed by"
        "  - a single (byte ..) or (word ..) constant with comments"
        "* constants / tag bytes (by value)"
        "* constants / tag bytes (by name)"
        "* constants / tag bytes (by group)"
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

(defun gb/racket--rgcd--org-zp->string (level plist)
  (or
   (ignore-errors
     (let* ((file-ref (plist-get plist :org-link))
            (zp-label (plist-get plist :label))
            (zp-address (plist-get plist :address))
            (zp-len  (plist-get plist :length))
            (docline-sep (make-string (min 0 (- 40 (length zp-address))) ?\ ))
            (doc-line (car (or (plist-get plist :doclines) (list "no doc"))))
            (racket-require (plist-get plist :racket-require))
            (assembler-include (plist-get plist :assembler-include))
            (indented-doc-lines (string-join (or (plist-get plist :doclines) (list "no doc")) "\n  ")))
       (format "- [[%s][%s]] :: %s %s %s\n  - len :: %s\n  - racket require :: %s\n  - assembler include :: %s\n  %s"
               file-ref
               zp-label
               zp-address
               docline-sep
               doc-line
               zp-len
               racket-require
               assembler-include
               indented-doc-lines)))
   (format "- ERROR ON PROCESSING %S" plist)))

(defun gb/racket--rgcd--org-ml-by-address->string (level plist)
  (gb/racket--rgcd--org-zp-by-address->string level plist))

(defun gb/racket--rgcd--org-ml->string (level plist)
  (gb/racket--rgcd--org-zp->string level plist))

(defun gb/racket--rgcd--org-c->string (level plist)
  (or
   (ignore-errors
     (let* ((file-ref (plist-get plist :org-link))
            (c-label (plist-get plist :label))
            (c-value (plist-get plist :address))
            (c-len  (plist-get plist :length))
            (docline-sep (make-string (min 0 (- 40 (length c-value))) ?\ ))
            (doc-line (car (or (plist-get plist :doclines) (list "no doc"))))
            (racket-require (plist-get plist :racket-require))
            (assembler-include (plist-get plist :assembler-include))
            (indented-doc-lines (string-join (or (plist-get plist :doclines) (list "no doc")) "\n  ")))
       (format "- [[%s][%s]] :: %s %s %s\n  - len :: %s\n  - racket require :: %s\n  - assembler include :: %s\n  %s"
               file-ref
               c-label
               c-value
               docline-sep
               doc-line
               c-len
               racket-require
               assembler-include
               indented-doc-lines)))
   (format "- ERROR ON PROCESSING %S" plist)))

(defun gb/racket--rgcd--org-c-by-value->string (level plist)
  (or
   (ignore-errors
     (let* ((file-ref (plist-get plist :org-link))
            (c-label (plist-get plist :label))
            (c-value (plist-get plist :address))
            (c-len  (plist-get plist :length))
            (docline-sep (make-string (min 0 (- 40 (length c-value))) ?\ ))
            (doc-line (car (or (plist-get plist :doclines) (list "no doc"))))
            (racket-require (plist-get plist :racket-require))
            (assembler-include (plist-get plist :assembler-include))
            (indented-doc-lines (string-join (or (plist-get plist :doclines) (list "no doc")) "\n  ")))
       (format "- [[%s][%s]] :: %s %s %s\n  - len :: %s\n  - racket require :: %s\n  - assembler include :: %s\n  %s"
               file-ref
               c-value
               c-label
               docline-sep
               doc-line
               c-len
               racket-require
               assembler-include
               indented-doc-lines)))
   (format "- ERROR ON PROCESSING %S" plist)))

(defun gb/racket--rgcd--org-zp-by-address->string (level plist)
  (or
   (ignore-errors
     (let* ((file-ref (plist-get plist :org-link))
            (zp-label (plist-get plist :label))
            (zp-address (plist-get plist :address))
            (zp-len  (plist-get plist :length))
            (docline-sep (make-string (min 0 (- 40 (length zp-address))) ?\ ))
            (doc-line (car (or (plist-get plist :doclines) (list "no doc"))))
            (racket-require (plist-get plist :racket-require))
            (assembler-include (plist-get plist :assembler-include))
            (indented-doc-lines (string-join (or (plist-get plist :doclines) (list "no doc")) "\n  ")))
       (format "- [[%s][%s]] :: %s %s %s\n  - len :: %s\n  - racket require :: %s\n  - assembler include :: %s\n  %s"
               file-ref
               zp-address
               zp-label
               docline-sep
               doc-line
               zp-len
               racket-require
               assembler-include
               indented-doc-lines)))
   (format "- ERROR ON PROCESSING %S" plist)))

;; (gb/racket--rgcd--org-zp-by-address->string 1 (list :doclines nil :address "$d9" :length "byte" :racket-require "(require (only-in \"vm-memory-map.rkt\" VM_MEMORY_MANAGEMENT_CONSTANTS))" :assembler-include "VM_MEMORY_MANAGEMENT_CONSTANTS" :org-link "file:vm-memory-map.rkt::63" :filename "vm-memory-map.rkt" :line "63" :column "7" :type "ZP" :label "ZP_TEMP3" :group (list "temp")))

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

(defconst gb/racket--rgcd--fun-by-name-regex
  "^* functions \(by name\)"
  "function by name org header for regex search")

(defconst gb/racket--rgcd--fun-by-group-regex
  "^* functions \(by group\)"
  "function by name org header for regex search")

(defconst gb/racket--rgcd--zp-by-address-regex
  "* zero-page-locations \(by address\)")

(defconst gb/racket--rgcd--zp-by-name-regex
  "* zero-page-locations \(by name\)")

(defconst gb/racket--rgcd--zp-by-group-regex
  "* zero-page-locations \(by group\)")

(defconst gb/racket--rgcd--ml-by-address-regex
  "* memory-locations \(by address\)")

(defconst gb/racket--rgcd--ml-by-name-regex
  "* memory-locations \(by name\)")

(defconst gb/racket--rgcd--ml-by-group-regex
  "* memory-locations \(by group\)")

(defconst gb/racket--rgcd--c-by-value-regex
  "* constants / tag bytes \(by value\)")

(defconst gb/racket--rgcd--c-by-name-regex
  "* constants / tag bytes \(by name\)")

(defconst gb/racket--rgcd--c-by-group-regex
  "* constants / tag bytes \(by group\)")

(defun gb/racket--rgcd--gen-org-zp (plist org-file)
  "generate all entries for azp constant described by PLIST int ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgcd--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgcd--zp-by-address-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-zp-by-address->string 1 plist))
    (save-buffer)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgcd--zp-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-zp->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgcd--zp-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgcd--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgcd--org-zp->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgcd--gen-org-c (plist org-file)
  "generate all entries for a constant described by PLIST int ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgcd--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgcd--c-by-value-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-c-by-value->string 1 plist))
    (save-buffer)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgcd--c-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-c->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgcd--c-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgcd--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgcd--org-c->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgcd--gen-org-ml (plist org-file)
  "generate all entries for azp constant described by PLIST int ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgcd--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgcd--ml-by-address-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-ml-by-address->string 1 plist))
    (save-buffer)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgcd--ml-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-ml->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgcd--ml-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgcd--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgcd--org-ml->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgcd--gen-org-fun (plist org-file)
  "generate all entries for function described by PLIST into ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgcd--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgcd--fun-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgcd--org-fun->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgcd--fun-by-group-regex)
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
    (mapc (lambda (plist) (gb/racket--rgcd--gen-org-zp plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "ZP")))
    (mapc (lambda (plist) (gb/racket--rgcd--gen-org-ml plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "M")))
    (mapc (lambda (plist) (gb/racket--rgcd--gen-org-c plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "C")))
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
  (string-match "^ *;; *@DC-\\([^:]*\\): *\\([^ ,]+\\)\\(, *group: *\\([^ ,]*\\)\\)?" match-line)
  `(:type  ,(match-string 1 match-line)
    :label ,(match-string 2 match-line)
    :group ,(or (when (match-string 4 match-line) (split-string (match-string 4 match-line) "-")) (list))))

;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages-sub-a")
;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages-sub-a")
;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages")
;; (gb/racket--rgdc-match--label ";; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER")
