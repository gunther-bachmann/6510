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
   #'gb/racket--rgdc--enrich-plist-b
   (mapcar
    #'gb/racket--rgdc--enrich-plist-c
    (mapcar
     #'gb/racket--rgdc--enrich-plist-ml
     (mapcar
      #'gb/racket--rgdc--enrich-plist-zp
      (mapcar
       #'gb/racket--rgdc--enrich-plist-fun
       dc-plists))))))

(defun gb/racket--rgdc--get-match-in-file (filename line regexp forward)
  "go into FILENAME to LINE end of line, search for REGEXP forward if FORWARD is t, else backward
and return the match 1"
  (save-current-buffer
    (when (file-exists-p filename)
      (with-temp-buffer
        (find-file filename)
        (goto-line line)
        (end-of-line)
        (if forward
            (search-forward-regexp regexp nil t)
          (search-backward-regexp regexp nil t))
        (substring-no-properties (match-string 1))))))

(defconst gb/racket--rgdc--byte-word-const-regex "\\(byte\\|word\\) +\\$[a-fA-F0-9]+")
(defun gb/racket--rgdc--following-len (filename line)
  "return the length of this assembler constant (either byte or word)"
  (ignore-errors
    (gb/racket--rgdc--get-match-in-file filename (string-to-number line) gb/racket--rgdc--byte-word-const-regex t)))

(defconst gb/racket--rgdc--define-regex-const "(define +[A-Za-z0-9_-]+ +#x\\([a-fA-F0-9]+\\)")
(defun gb/racket--rgdc--following-code (filename line)
  "get the value of the following definition (byte code)"
  (ignore-errors
    (gb/racket--rgdc--get-match-in-file filename (string-to-number line) gb/racket--rgdc--define-regex-const t)))

(defun gb/racket--rgdc--next-define (filename line)
  "get the next define starting the search from LINE in FILENAME"
  (gb/racket--rgdc--get-match-in-file filename (string-to-number line) gb/racket--rgdc--define-regex t))

(defun gb/racket--rgdc--over-next-define (filename line)
  (gb/racket--rgdc--get-file-lines filename (+ 1 (string-to-number line)) t gb/racket--rgdc--define-regex-const 2 t))

(defun gb/racket--rgdc--surrounding-define (filename line)
  "get the define by searching backward from the given LINE in the FILENAME"
  (gb/racket--rgdc--get-match-in-file filename (string-to-number line) gb/racket--rgdc--define-regex nil))

(defun gb/racket--rgdc--zp-doclines (filename line)
  "get the next doclines until the second word/byte-const follows starting from LINE in FILENAME"
  (gb/racket--rgdc--get-file-lines
   filename
   (+ 1 (string-to-number line))
   t
   gb/racket--rgdc--byte-word-val-regex
   1
   nil))

(defun gb/racket--rgdc--next-doclines (filename line)
  "get the next doclines until a define follows starting from LINE in FILENAME"
  (gb/racket--rgdc--get-file-lines
   filename
   (+ 1 (string-to-number line))
   t
   gb/racket--rgdc--define-regex
   1
   t))

(defun gb/racket--rgdc--assconst-doclines (filename line)
  "find doc lines until next label | DC"
  (gb/racket--rgdc--get-file-lines filename (+ 1 (string-to-number line)) t gb/racket--rgdc--byte-word-const-regex 1 nil))

(defun gb/racket--rgdc--get-file-lines
    (filename line line-beg regexp n-times after-line-beg)
  "if LINE-BEG, else to the end-of-line
then mark the current point and search the REGEXP N-TIMES and return the text up to there
(not including the final line)"
    (save-current-buffer
      (when (file-exists-p filename)
        (with-temp-buffer
          (find-file filename)
          (goto-line line)
          (if line-beg
              (beginning-of-line)
            (end-of-line))
          (let ((beg (point)))
            (--dotimes  n-times (search-forward-regexp regexp nil t))
            (if after-line-beg
                (beginning-of-line)
              (end-of-line))
            (buffer-substring-no-properties beg (point)))))))

(defun gb/racket--rgdc--cleanup-doc-lines (doc-lines)
  "filter the following doc lines from noise

filter all after (next) \"@DC-\" definition
remove ;; prefix
filter lines that start with ---"
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

;; (gb/racket--rgdc--cleanup-doc-lines
;;  (list "(some code) ;; line one"
;;        "; line two"
;;        "(line without comment)"
;;        ";;@DC-C other"   ;; ignore this line, don't take anything here after
;;        ";line three"
;;        ";;line four"
;;        "(define "       ;; don't take from hereon
;;        ";; line five"
;;        ";;@DC-C some"))

;; (gb/racket--rgdc--cleanup-doc-lines
;;  (list
;;   "   ;; zero page location 3 for temp usage"
;;   "(byte-const ZP_TEMP3                  $d9) ;; may be used as pointer (in combination with ZP_TEMP4 => must be in adjacent memory locations)"))

(defun gb/racket--rgdc--this-docline (filename line)
  "read the doc within the line"
  "UNKNOWN DOC")

(defconst gb/racket--rgdc--byte-word-val-regex "\\(byte\\|word\\)-const *[A-Za-z0-9_-]+ *\\(\\$[a-fA-F0-9]+\\)")
(defun gb/racket--rgdc--following-address (filename line)
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

(defun gb/racket--rgdc--enrich-plist-b (plist)
  (if (not (string= "B" (plist-get plist :type)))
      plist
    (message "enriching byte code")
    (gb/racket--rgdc--enrich-plist-bytecode plist)))

(defun gb/racket--rgdc--enrich-plist-ml (plist)
  (if (not (string= "M" (plist-get plist :type)))
      plist
    (message "enriching memory location")
    (gb/racket--rgdc--enrich-plist-assconst plist)))

(defun gb/racket--rgdc--enrich-plist-c (plist)
  (if (not (string= "C" (plist-get plist :type)))
      plist
    (message "enriching constant")
    (gb/racket--rgdc--enrich-plist-constant plist)))

(defun gb/racket--rgdc--enrich-plist-zp (plist)
  "enrich the given PLIST with zero page data (if it is of type ZP)"
  (if (not (string= "ZP" (plist-get plist :type)))
      plist
    (message "enriching zero page")
    (gb/racket--rgdc--enrich-plist-constant plist)))

(defconst gb/racket--rgdc--word-const-regex "word +\\$[a-fA-F0-9]+")
(defconst gb/racket--rgdc--byte-const-regex "byte +\\$[a-fA-F0-9]+")
(defconst gb/racket--rgdc--org-command "(org #x\\([a-fA-F0-9]+\\))")
(defun gb/racket--rgdc--find-address-by-org (filename line)
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

(defun gb/racket--rgdc--enrich-plist-assconst (plist)
  (let ((surrouding-define-label (or
                                  (gb/racket--rgdc--surrounding-define
                                   (plist-get plist :filename)
                                   (plist-get plist :line))
                                  "UNKNOWN"))
        (address (or (gb/racket--rgdc--find-address-by-org
                     (plist-get plist :filename)
                     (plist-get plist :line))
                    "UNKNOWN"))
        (len (or (gb/racket--rgdc--following-len
                 (plist-get plist :filename)
                 (plist-get plist :line))
                "UNKNOWN")))
    (message (format "found address %s" address))
    `( :doclines ,(gb/racket--rgdc--cleanup-doc-lines
                   (split-string (gb/racket--rgdc--assconst-doclines
                                  (plist-get plist :filename)
                                  (plist-get plist :line))
                                 "\n"))
       :address ,address
       :length ,len
       :racket-require ,(format "(require (only-in \"%s\" %s))" (plist-get plist :filename) surrouding-define-label)
       :assembler-include ,surrouding-define-label
       :org-link ,(format "file:%s::%s" (plist-get plist :filename) (plist-get plist :line))
       ,@plist)))

(defun gb/racket--rgdc--enrich-plist-bytecode (plist)
  (let ((label
         (or
          (gb/racket--rgdc--next-define
           (plist-get plist :filename)
           (plist-get plist :line))
          "UNKNOWN"))
        (code (or (gb/racket--rgdc--following-code
                           (plist-get plist :filename)
                           (plist-get plist :line))
                 "UNKNOWN")))
    `( :doclines ,(gb/racket--rgdc--cleanup-doc-lines
                   (split-string (gb/racket--rgdc--over-next-define
                                  (plist-get plist :filename)
                                  (plist-get plist :line))
                                 "\n"))
       :code ,code
       :racket-require ,(format "(require (only-in \"%s\" %s))" (plist-get plist :filename) label)
       :assembler-include ,(format "BC_%s" label)
       :org-link ,(format "file:%s::%s" (plist-get plist :filename) (plist-get plist :line))
       ,@plist)))

(defun gb/racket--rgdc--enrich-plist-constant (plist)
  (let ((surrouding-define-label
         (or
          (gb/racket--rgdc--surrounding-define
           (plist-get plist :filename)
           (plist-get plist :line))
          "UNKNOWN"))
        (address-n-len (or (gb/racket--rgdc--following-address
                           (plist-get plist :filename)
                           (plist-get plist :line))
                          (list "UNKNOWN" "UNKNOWN"))))
    `( :doclines ,(gb/racket--rgdc--cleanup-doc-lines
                   (split-string (gb/racket--rgdc--zp-doclines
                                  (plist-get plist :filename)
                                  (plist-get plist :line))
                                 "\n"))
       :address ,(car address-n-len)
       :length ,(cadr address-n-len)
       :racket-require ,(format "(require (only-in \"%s\" %s))" (plist-get plist :filename) surrouding-define-label)
       :assembler-include ,surrouding-define-label
       :org-link ,(format "file:%s::%s" (plist-get plist :filename) (plist-get plist :line))
       ,@plist)))

(defun gb/racket--rgdc--enrich-plist-fun (plist)
  "enrich the given PLIST with function data (if it is of type FUN)"
  (if (not (string= "FUN" (plist-get plist :type)))
      plist
    (message "enrich functions")
    (let ((next-define-label
           (or
            (gb/racket--rgdc--next-define
             (plist-get plist :filename)
             (plist-get plist :line))
            "UNKNOWN")))
      `( :doclines ,(gb/racket--rgdc--cleanup-doc-lines
                     (split-string (gb/racket--rgdc--next-doclines
                                    (plist-get plist :filename)
                                    (plist-get plist :line))
                                   "\n"))
         :racket-require ,(format "(require (only-in \"%s\" %s))" (plist-get plist :filename) next-define-label)
         :assembler-include ,next-define-label
         :org-link ,(format "file:%s::%s" (plist-get plist :filename) (plist-get plist :line))
         ,@plist))))

;; (gb/racket--rgdc--enrich-plist-fun (gb/racket--rgdc-line-to-plist "repo/+1/6510/src/nmil/vm-pages.rkt:176:4:;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages"))

(defconst gb/racket--rgdc--org-template
  (list "#+title: vm-object-index"
        "* file description"
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
        "- @DC-B :: byte code"
        "  @DC-B: <bc-label>(, group: <b-group>(-<b-group>)*)"
        "  example: ;; @DC-B: BINC, group: byt"
        "           ;; @DC-B: GET_AF_0, group: cell_array"
        "  the file name is used for creating the racket require line"
        "  the following define is used for racket require line and to extract the actual byte code"
        "  the next define (starting with BC_) is used for assembler include"
        "  the @DC-B line is used for the org-link"
        "  the line following this @DC-B comment must:"
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
        "* byte codes (by code)"
        "* byte codes (by name)"
        "* byte codes (by group)"
        "* - :noexport:"
        "#+begin_src emacs-lisp"
        "  ;; Local Variables:"
        "  ;; org-pretty-entities-include-sub-superscripts: nil"
        "  ;; End:"
        "#+end_src"
        ))

(defun gb/racket--rgdc--org-file-create (org-file)
  (with-temp-buffer
    (mapc #'(lambda (line) (insert (format "%s\n" line))) gb/racket--rgdc--org-template)
    (write-file org-file nil)))

;; (gb/racket--rgdc--org-file-create "some.org")

(defun gb/racket--rgdc--org-bc-by-code->string (level plist)
  (or
   (ignore-errors
     (let* ((file-ref (plist-get plist :org-link))
            (label (plist-get plist :label))
            (label-docline-sep (make-string (min 0 (- 40 (length label))) ?\ ))
            (bytecode (plist-get plist :code))
            (doc-line (car (or (plist-get plist :doclines) (list "no doc"))))
            (racket-require (plist-get plist :racket-require))
            (assembler-include (plist-get plist :assembler-include))
            (indented-doc-lines (string-join (or (plist-get plist :doclines) (list "no doc")) "\n  ")))
       (format "- [[%s][$%s]] :: %s %s %s\n  - racket require :: %s\n  - assembler include :: %s\n  %s"
               file-ref
               bytecode
               label
               label-docline-sep
               doc-line
               racket-require
               assembler-include
               indented-doc-lines)))
   (format "- ERROR ON PROCESSING %S" plist)))

(defun gb/racket--rgdc--org-bc->string (level plist)
  (or
   (ignore-errors
     (let* ((file-ref (plist-get plist :org-link))
            (label (plist-get plist :label))
            (label-docline-sep (make-string (min 0 (- 40 (length label))) ?\ ))
            (bytecode (plist-get plist :code))
            (doc-line (car (or (plist-get plist :doclines) (list "no doc"))))
            (racket-require (plist-get plist :racket-require))
            (assembler-include (plist-get plist :assembler-include))
            (indented-doc-lines (string-join (plist-get plist :doclines) "\n  ")))
       (format "- [[%s][%s]] :: $%s %s %s\n  - racket require :: %s\n  - assembler include :: %s\n  %s"
               file-ref
               label
               bytecode
               label-docline-sep
               doc-line
               racket-require
               assembler-include
               indented-doc-lines)))
   (format "- ERROR ON PROCESSING %S" plist)))

(defun gb/racket--rgdc--org-fun->string (level plist)
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

(defun gb/racket--rgdc--org-zp->string (level plist)
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

(defun gb/racket--rgdc--org-ml-by-address->string (level plist)
  (gb/racket--rgdc--org-zp-by-address->string level plist))

(defun gb/racket--rgdc--org-ml->string (level plist)
  (gb/racket--rgdc--org-zp->string level plist))

(defun gb/racket--rgdc--org-c->string (level plist)
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

(defun gb/racket--rgdc--org-c-by-value->string (level plist)
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

(defun gb/racket--rgdc--org-zp-by-address->string (level plist)
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

;; (gb/racket--rgdc--org-zp-by-address->string 1 (list :doclines nil :address "$d9" :length "byte" :racket-require "(require (only-in \"vm-memory-map.rkt\" VM_MEMORY_MANAGEMENT_CONSTANTS))" :assembler-include "VM_MEMORY_MANAGEMENT_CONSTANTS" :org-link "file:vm-memory-map.rkt::63" :filename "vm-memory-map.rkt" :line "63" :column "7" :type "ZP" :label "ZP_TEMP3" :group (list "temp")))

(defun gb/racket--rgdc--loop-over-lists-and-sort ()
  (save-excursion
    (goto-char 0)
    (search-forward-regexp "^\\* constants " nil t) ;; first entry to start sorting
    (while (search-forward-regexp "^- " nil t)
      (org-sort-list nil ?a nil nil nil)
      (search-forward-regexp "^\\* " nil t))
    (save-buffer)))

(defun gb/racket--rgdc--jump-to-group (groups level max-point)
  "search forward to find the given group but don't leave the current heading!"
  (when groups
    (let ((group-entry (format "%s %s" (make-string level ?\*) (car groups))))
      (message (format "group-entry: %s" group-entry))
      (unless (search-forward group-entry max-point t)
        (insert (format "\n%s %s" (make-string level ?\*) (car groups))))
      (gb/racket--rgdc--jump-to-group (cdr groups) (+ 1 level) max-point))))

(defconst gb/racket--rgdc--fun-by-name-regex
  "^* functions \(by name\)"
  "function by name org header for regex search")

(defconst gb/racket--rgdc--fun-by-group-regex
  "^* functions \(by group\)"
  "function by name org header for regex search")

(defconst gb/racket--rgdc--zp-by-address-regex
  "* zero-page-locations \(by address\)")

(defconst gb/racket--rgdc--zp-by-name-regex
  "* zero-page-locations \(by name\)")

(defconst gb/racket--rgdc--zp-by-group-regex
  "* zero-page-locations \(by group\)")

(defconst gb/racket--rgdc--ml-by-address-regex
  "* memory-locations \(by address\)")

(defconst gb/racket--rgdc--ml-by-name-regex
  "* memory-locations \(by name\)")

(defconst gb/racket--rgdc--ml-by-group-regex
  "* memory-locations \(by group\)")

(defconst gb/racket--rgdc--c-by-value-regex
  "* constants / tag bytes \(by value\)")

(defconst gb/racket--rgdc--c-by-name-regex
  "* constants / tag bytes \(by name\)")

(defconst gb/racket--rgdc--c-by-group-regex
  "* constants / tag bytes \(by group\)")

(defun gb/racket--rgdc--gen-org-zp (plist org-file)
  "generate all entries for azp constant described by PLIST int ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgdc--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--zp-by-address-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-zp-by-address->string 1 plist))
    (save-buffer)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--zp-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-zp->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgdc--zp-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgdc--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgdc--org-zp->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgdc--gen-org-c (plist org-file)
  "generate all entries for a constant described by PLIST int ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgdc--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--c-by-value-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-c-by-value->string 1 plist))
    (save-buffer)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--c-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-c->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgdc--c-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgdc--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgdc--org-c->string 1 plist)))
        (save-buffer)))))

(defconst gb/racket--rgdc--bc-by-code-regex
  "* byte codes \(by code\)")

(defconst gb/racket--rgdc--bc-by-name-regex
  "* byte codes \(by name\)")

(defconst gb/racket--rgdc--bc-by-group-regex
  "* byte codes \(by group\)")

(defun gb/racket--rgdc--gen-org-bytecode (plist org-file)
  "generate all entries for a constant described by PLIST int ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgdc--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--bc-by-code-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-bc-by-code->string 1 plist))
    (save-buffer)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--bc-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-bc->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgdc--bc-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgdc--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgdc--org-bc->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgdc--gen-org-ml (plist org-file)
  "generate all entries for azp constant described by PLIST int ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgdc--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--ml-by-address-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-ml-by-address->string 1 plist))
    (save-buffer)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--ml-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-ml->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgdc--ml-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgdc--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgdc--org-ml->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgdc--gen-org-fun (plist org-file)
  "generate all entries for function described by PLIST into ORG-FILE"
  (unless (file-exists-p org-file)
    (gb/racket--rgdc--org-file-create org-file))
  (with-current-buffer (find-file-noselect org-file)
    (goto-char 0)
    (search-forward-regexp gb/racket--rgdc--fun-by-name-regex)
    (end-of-line)
    (insert "\n")
    (insert (gb/racket--rgdc--org-fun->string 1 plist))
    (save-buffer)
    (when (plist-get plist :group)
      (goto-char 0)
      (search-forward-regexp gb/racket--rgdc--fun-by-group-regex)
      (end-of-line)
      (let ((max-point-for-search
             (save-excursion
               (search-forward-regexp "^* ")
               (point))))
        (gb/racket--rgdc--jump-to-group (plist-get plist :group) 2 max-point-for-search)
        (insert (format "\n%s" (gb/racket--rgdc--org-fun->string 1 plist)))
        (save-buffer)))))

(defun gb/racket--rgdc ()
  "execute documentation generation within this folder (and subfolders)"
  (interactive)
  (let ((file-name (read-string "org file name to generate: " "vm-object-index.org")))
    (when (file-exists-p file-name)
      (delete-file file-name nil))
    (mapc (lambda (plist) (gb/racket--rgdc--gen-org-fun plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "FUN")))
    (mapc (lambda (plist) (gb/racket--rgdc--gen-org-zp plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "ZP")))
    (mapc (lambda (plist) (gb/racket--rgdc--gen-org-ml plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "M")))
    (mapc (lambda (plist) (gb/racket--rgdc--gen-org-c plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "C")))
    (mapc (lambda (plist) (gb/racket--rgdc--gen-org-bytecode plist file-name))
          (gb/racket--rgdc--enrich
           (gb/racket--rip-grep-dc "B")))
    (with-temp-buffer (find-file file-name)
                      (gb/racket--rgdc--loop-over-lists-and-sort))))

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

;; (gb/racket--rgdc-line-to-plist "src/nmil/vm-pages.rkt:176:4:;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages")

;; (plist-get (gb/racket--rgdc-line-to-plist "src/nmil/vm-pages.rkt:176:4:;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages") :filename)

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
