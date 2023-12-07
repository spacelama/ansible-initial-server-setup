;;faces
;;debugging - run these before adding new stuff:
(remove-hook 'font-lock-mode-hook 'turn-on-keywords-case-fold-search)
(remove-hook 'font-lock-mode-hook 'add-fixme-highlighting)
(remove-hook 'font-lock-mode-hook 'add-math-highlighting)
(remove-hook 'font-lock-mode-hook 'add-number-highlighting)
(remove-hook 'font-lock-mode-hook 'add-specific-mode-hooks-hack)
(remove-hook 'font-lock-mode-hook 'add-mail-highlighting)
;(remove-hook 'font-lock-mode-hook 'font-lock-fontify-buffer)

(remove-hook 'find-file-hooks 'gnuplot-mmm-init)
(remove-hook 'find-file-hooks 'latex-mmm-init t)
(remove-hook 'find-file-hooks 'perl-mmm-init t)
(remove-hook 'find-file-hooks 'turn-on-font-lock-mode t)

; '(message-header-name-face ((t (background dark)) (:foreground "DarkGreen")) (((class color) (background light)) (:foreground "cornflower blue")) (t (:bold t)))
; '(message-header-subject-face ((t (background dark)) (:foreground "green3")) (((class color) (background light)) (:foreground "navy blue" :bold t)) (t (:bold t)))
; '(message-header-newsgroups-face ((t (background dark)) (:foreground "yellow" :bold t :italic t)) (((class color) (background light)) (:foreground "blue4" :bold t :italic t)) (t (:bold t :italic t)))
; '(message-header-other-face ((t (background dark)) (:foreground "#b00000")) (((class color) (background light)) (:foreground "steel blue")) (t (:bold t :italic t)))
; '(message-cited-text-face ((t (background dark)) (:foreground "red")) (((class color) (background light)) (:foreground "red")) (t (:bold t)))

(defface font-lock-number-face
  '((t :foreground "#ff9999"
       ))
  "timc: face for numbers"
  :group 'basic-faces)
;; now make it a variable. (you shouldn't do this)
;(defvar font-lock-number-face "my face too")

;(defface font-lock-constant-face
;  '((t :foreground "#ffcccc"
;       ))
;  "timc: face for constants"
;  )

(defface font-lock-math-face
  '((t :foreground "#aa66cc"
       ))
  "timc: face for maths"
  :group 'basic-faces)
;; now make it a variable. (you shouldn't do this)
;(defvar font-lock-math-face "my face too")

(defface bufperlface
  '((t :foreground "#ee4444"
       ))
  "timc: face for buf"
  :group 'basic-faces)


(defface bufemacslispface
  '((t :foreground "#ee4400"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufshellscriptface
  '((t :foreground "#ee4488"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufwinmgrface
  '((t :foreground "#004488"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface buftextface
  '((t :foreground "#ee8888"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufhtmlface
  '((t :foreground "#eeaa44"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface buflatexface
  '((t :foreground "#ee0088"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufbibtexface
  '((t :foreground "#ee0044"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufmakefileface
  '((t :foreground "#eebbbb"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufcface
  '((t :foreground "#eebb88"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufforface
  '((t :foreground "#eebbff"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface bufremoteface
  '((t :foreground "#88bbff"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface remote-face1
  '((t :foreground "#667744"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface datafileface
  '((t :foreground "#aa6677"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface remote-face2
  '((t :foreground "#aa22ff"
       ))
  "timc: face for buf"
  :group 'basic-faces)

(defface remote-face3
  '((t :foreground "#eebb44"
       ))
  "timc: face for buf"
  :group 'basic-faces)


;;functions

(defun turn-on-font-lock-mode ()
  "Turns on font-lock-mode"
  (font-lock-mode t))

(defun turn-on-keywords-case-fold-search ()
  "Turn on font-lock-keywords-case-fold-search"
  (setq-default font-lock-keywords-case-fold-search t))

;;for regexps see -- eg
;;http://dp.iit.bme.hu/mosml/doc/telepites-emacs-sml.txt and
;;http://www.emacswiki.org/cgi-bin/wiki.pl?AddKeywords

;;number 0 or 1 seems to be bracketting level within the regexp to highlight

;;can use . font-lock-number-face)))) instead of (0 'font-lo...., but this seems to negate the affect of comments
(defun add-fixme-highlighting ()
  "Turn on extra highlighting for 'FIXME' and the like."
  (interactive)
  (font-lock-add-buffer-keywords
;   (list '("\\<\\(\\(FIXME\\|TODO\\|WARNING\\|XXX+\\):.*\\)" (0 'font-lock-warning-face t))))
;  (font-lock-add-buffer-keywords
;   (list '("\\(\\\\fixme{[^}]*}?\\)" (0 'font-lock-warning-face t)))) 
   '(("\\<\\(\\(FIXME\\|TODO\\|WARNING\\|XXX+\\):.*\\)" 0 font-lock-warning-face t)
    ("\\(\\\\fixme{[^}]*}?\\)" 0 font-lock-warning-face t)))
  )

(defun add-math-highlighting ()
  "Turn on extra highlighting for 'COS()' and the like."
  (interactive)
  (font-lock-add-buffer-keywords
;   (list '("\\<\\(d?a?\\(cos\\|sin\\|tan\\|tan\\|tan2\\)\\(deg\\)?\\|\\(d\\|f\\|l\\)?\\(sqrt\\|sqr\\|exp\\|exp10\\|abs\\|round\\|int\\|nint\\|min\\|max\\|sign\\|log\\|log10\\)\\)\\(f\\|h\\)?\\>"
;           (0 'font-lock-math-face append))))
   '(("\\<\\(d?a?\\(cos\\|sin\\|tan\\|tan\\|tan2\\)\\(deg\\)?\\|\\(d\\|f\\|l\\)?\\(sqrt\\|sqr\\|exp\\|exp10\\|abs\\|round\\|int\\|nint\\|min\\|max\\|sign\\|log\\|log10\\)\\)\\(f\\|h\\)?\\>"
           0 'font-lock-math-face append)))
  )




(defun add-number-highlighting ()
  "Turn on extra highlighting for numbers."
  (interactive)
  (font-lock-add-buffer-keywords
                                             ;                                                            __fortran 0.6d0 format
;   (list '("-?\\<\\(\\(0[xX][0-9a-fA-F]+[lL]?\\|[0-9]+\\.?[0-9]*\\([eEdD][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?0?\\)\\|M_PI\\)\\>"
;           . 'font-lock-number-face)))
   '(("-?\\<\\(\\(0[xX][0-9a-fA-F]+[lL]?\\|[0-9]+\\.?[0-9]*\\([eEdD][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?0?\\)\\|M_PI\\)\\>"
           0 'font-lock-number-face append)))
  )

(defun add-mail-highlighting ()
  "Turns on extra highlighting for mail and News messages"
; idea from http://www.robf.de/Hacking/elisp/rf-vm.el but concat is just not working --
  ;;(list (concat "^" (make-string 76 ?.)
  ;;       "\\(.+$\\)")

  (interactive)
  (font-lock-add-buffer-keywords
   ;;colors stolen from message-mode
   '(("^\\([Tt]o:\\)[ 	]*\\(.+\\(\n[ 	].*\\)*\\)\n?" (1 (quote message-header-name-face)) (2 (quote message-header-to-face) nil t))
     ("^\\(^[GBF]?[Cc][Cc]:\\|^[Rr]eply-[Tt]o:\\)[ 	]*\\(.+\\(\n[ 	].*\\)*\\)\n?" (1 (quote message-header-name-face)) (2 (quote message-header-cc-face) nil t))
     ("^\\([Ss]ubject:\\)[ 	]*\\(.+\\(\n[ 	].*\\)*\\)\n?" (1 (quote message-header-name-face)) (2 (quote message-header-subject-face) nil t))
     ("^\\([Nn]ewsgroups:\\|Followup-[Tt]o:\\)[ 	]*\\(.+\\(\n[ 	].*\\)*\\)\n?" (1 (quote message-header-name-face)) (2 (quote message-header-newsgroups-face) nil t))
     ("^\\([A-Z][^: \n	]+:\\)[ 	]*\\(.+\\(\n[ 	].*\\)*\\)\n?" (1 (quote message-header-name-face)) (2 (quote message-header-other-face) nil t))
     ("^\\(X-[A-Za-z0-9-]+\\|In-Reply-To\\):[ 	]*\\(.+\\(\n[ 	].*\\)*\\)\n?" (1 (quote message-header-name-face)) (2 (quote message-header-name-face)))
     ("^\\(--text follows this line--\\)$" 1 (quote message-separator-face))
     ("^[ 	]*\\([A-Za-z]+[A-Za-z0-9_.@-]*\\)?[:>|}].*" (0 (quote message-cited-text-face)))
     ("<#/?\\(multipart\\|part\\|external\\|mml\\).*>" (0 (quote message-mml-face)))
     ;;these colours are my own
     ("^............................................................................\\(.+$\\)"
      (1 (quote font-lock-warning-face append)))
     ("^-- $" (0 (quote font-lock-comment-face)))
     ) )
  )

(defun add-fortran-reserved-keywords ()
  "Turn on extra highlighting for keywords, under fortran."
  (interactive)
  (font-lock-add-buffer-keywords
   (list '("\\<\\(allocatable\\|allocate\\|deallocate\\)\\>" (0 'font-lock-keyword-face append)))) 
  )
(defun add-f90-reserved-keywords ()
  "Turn on extra highlighting for keywords, under fortran."
  (interactive)
  (font-lock-add-buffer-keywords
   (list '("^[ 	0-9]*\\(double *precision\\)\\(.*::\\|[ 	]*(.*)\\)?\\([^!\n]*\\)" (1 'font-lock-type-face) (3 'font-lock-variable-name-face)))) 
  )

(defun add-bash-shell-reserved-keywords ()
  "Turn on extra highlighting for keywords, under bash."
  (interactive)
  (font-lock-add-buffer-keywords
   (list '("^\\(function\\)\\> *\\([^( ]*\\) *(" (1 'font-lock-keyword-face append) (2 'font-lock-function-name-face append)))
   (list '("^\\([^ \n]*\\)\\> *(" (1 'font-lock-function-name-face append)))
   ) 
  )


(defun add-specific-mode-hooks-hack ()
  "nasty hack because setting font-lock-keywords or
fortran-font-lock-keywords within a fortran-mode-hook doesn't seem to
work - in one case the keywords aren't set, and in other case they
override everything but what we explicitly set. Tried different
settings of append/prepend/t/nil/keep with no luck."
  (if (string-equal (format-mode-line mode-name) "Fortran")
      (add-fortran-reserved-keywords))
  (if (string-equal (format-mode-line mode-name) "F90")
      (add-f90-reserved-keywords))
  (if (string-equal (format-mode-line mode-name) "Shell-script")
      (add-bash-shell-reserved-keywords))
  (if (string-equal (format-mode-line mode-name) "Mail")
      (add-mail-highlighting))
  )


;(defun pretty-lambdas ()x
;  (interactive)
;;;makify lambda's pretty :)
;  (font-lock-add-buffer-keywords
;   '(("\\<lambda\\>"
;          (1 (progn (compose-region (match-beginning 0) (match-end 0)
;                                    ,(make-char 'greek-iso8859-7 107))
;                    nil))))))



(add-hook 'font-lock-mode-hook 'turn-on-keywords-case-fold-search t)
;(add-hook 'c-mode-common-hook 'add-fixme-highlighting)
(add-hook 'font-lock-mode-hook 'add-fixme-highlighting t)
(add-hook 'font-lock-mode-hook 'add-math-highlighting t)
(add-hook 'font-lock-mode-hook 'add-number-highlighting t)
(add-hook 'font-lock-mode-hook 'add-specific-mode-hooks-hack t)
(add-hook 'find-file-hooks 'turn-on-font-lock-mode t)

(defun gnuplot-mmm-init ()    ;;Even if no submode found, it seems to
			      ;;put the whole file partly in gnuplot
			      ;;mode, which sucks, because then you
			      ;;get the menu at top.
  (save-excursion 
    (goto-char (point-min))
    (if (search-forward "begin gnuplot code:" nil t)
        (progn (mmm-ify-by-regexp 'gnuplot-mode "begin gnuplot code:" 1 "end gnuplot code:" -1 1)
               (message "Found gnuplot submode in file"))
      t)))

(defun latex-mmm-init ()
  (save-excursion 
    (goto-char (point-min))
    (if (search-forward "begin latex code:" nil t)
        (progn (mmm-ify-by-regexp 'latex-mode "begin latex code:" 1 "end latex code:" -1 1)
               (message "Found latex submode in file"))
      t)))

(defun perl-mmm-init ()
  (save-excursion 
    (goto-char (point-min))
    (if (search-forward "begin perl code:" nil t)
        (progn (mmm-ify-by-regexp 'perl-mode "begin perl code:" 1 "end perl code:" -1 1)
               (message "Found perl submode in file"))
      t)))

;;mmm automatic setup, so we can include the strings below in a
;;segment, and its mode will be automatically set
(add-hook 'find-file-hooks 'gnuplot-mmm-init t)
(add-hook 'find-file-hooks 'latex-mmm-init t)
(add-hook 'find-file-hooks 'perl-mmm-init t)

;;WARNING: add this one after all fontifications!

;(add-hook 'find-file-hooks 'font-lock-fontify-buffer t) ;;need to fontify again, since it seems to be already formatted by the time we get here



;;c-mode-common-hook?
;(add-hook 'c-mode-common-hook 'add-number-highlighting)
;(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)




;;Get c-x c-b to do funky colors.

(defconst Electric-buffer-menu-mode-font-lock-keywords
  (purecopy
   (list
    '("^ MR (Name|Buffer).*"       . font-lock-preprocessor-face) ;hdr 1
    '("^ -- ----.*"                . font-lock-preprocessor-face) ;hdr 2
;       '("/.*@.*"                 . bufremoteface) ; remote connections file
    '("/\\[.*swin.edu.au\\]"       . remote-face1)
    '("/\\[.*puzzling.org\\]"      . remote-face2)
    '("/\\[.*hexane.*\\]"          . remote-face3)
    '("/\\[.*\\]"                  . remote-face1)
    '("^\\(....Man: .*\\)"         1 font-lock-variable-name-face t) ;Manpg (new)
    '("^[. ][*][^%].[^*].*"        . font-lock-comment-face) ;Mod x temp
    '("^....[*]Buffer List[*].*"   . font-lock-doc-string-face) ;Buffer list
    '("^\\(....[*]shell.*\\)"      1 font-lock-reference-face t) ;shell buff
    '("^....[*].*"                 . font-lock-string-face) ;Temp buffer
    '("^....[+].*"                 . font-lock-keyword-face) ;Mail buffer
    '("^....[A-Za-z0-9/]*[-][+].*" . font-lock-keyword-face) ;Mail buffer
    '(".*Dired"                    . font-lock-function-name-face)
    '(".*CPerl"                    . bufperlface) ; Perl source file
    '(".*Emacs[^ ]*"               . bufemacslispface) ; Emacs Lisp source file
    '(".*Shell[^ ]*"               . bufshellscriptface) ; 
    '(".*Text"                     . buftextface) ; 
    '(".*LaTeX"                    . buflatexface) ; 
    '(".*BibTeX"                   . bufbibtexface) ; 
    '(".*C "                       . bufcface) ; 
    '(".*Fortran"                  . bufforface) ; 
    '(".*[^ ]  Makefile"           . bufmakefileface) ; 
    '(".*\\.par.*"                 . datafileface) ; 
    )))

(setq ibuffer-fontification-alist '(
        (45 (string-match-nonil "^/\\[.*\\(puzzling.org\\)\\].*$" (buffer-file-name)) 'remote-face2)
        (45 (string-match-nonil "^/\\[.*\\(hexane\\|tellurium\\|te\\|radium\\|ra\\|tera\\|mono\\|aer\\|ignis\\|dirac\\|gamow\\|maxwell\\|ant\\)[.a-zA-Z0-9]*\\].*$" (buffer-file-name)) 'remote-face3)
        (45 (string-match-nonil ".*\\.par" (buffer-file-name)) 'datafileface)
        (35 (string-match-nonil "^/\\[.*\\].*$" (buffer-file-name)) 'remote-face1)
        (40 (string-match-nonil "^/\\[.*\\(astronomy.swin.edu.au\\)\\].*$" (buffer-file-name)) 'remote-face1)
        (30 (eq major-mode 'dired-mode) 'font-lock-keyword-face)
        (30 (or (eq major-mode 'fortran-mode) (eq major-mode 'f90-mode)) 'bufforface)
        (30 (eq major-mode 'c-mode) 'bufcface)
        (30 (eq major-mode 'winmgr-mode) 'bufwinmgrface)
        (30 (or (eq major-mode 'shell-script-mode) (eq major-mode 'sh-mode)) 'bufshellscriptface)
        (30 (eq major-mode 'latex-mode) 'buflatexface)
        (30 (eq major-mode 'bibtex-mode) 'bufbibtexface)
        (30 (eq major-mode 'fortran-mode) 'bufforface)
        (30 (or (eq major-mode 'shell-mode) (eq major-mode 'eshell-mode)) 'font-lock-reference-face)
        (30 (or (eq major-mode 'cperl-mode) (eq major-mode 'perl-mode)) 'bufperlface)
        (30 (eq major-mode 'text-mode) 'buftextface)
        (30 (eq major-mode 'html-mode) 'bufhtmlface)
        (30 (eq major-mode 'makefile-mode) 'bufmakefileface)
        (30 (eq major-mode 'emacs-lisp-mode) 'bufemacslispface)
        (25 (memq major-mode ibuffer-help-buffer-modes) 'font-lock-keyword-face)
        (20 (string-match "^ " (buffer-name)) 'font-lock-keyword-face)
        (15 (string-match "^*" (buffer-name)) 'font-lock-keyword-face)
        (10 buffer-read-only 'font-lock-keyword-face)
        ))

; This hook run after buffer formatted, so it is necessary to re-fontify it...
(add-hook 'electric-buffer-menu-mode-hook 'refontify-buffer)

(defun refontify-buffer ()
  "Turns on font-lock-mode and refontifies for buffers that need to be set up again"
  (font-lock-mode 1)
  (font-lock-fontify-buffer))

(when (boundp 'col-highlight-set-interval)
  (setq col-highlight-vline-face-flag nil)  ; use vline faces already defined
  (col-highlight-set-interval 0.02)
  (column-highlight-mode 1))
