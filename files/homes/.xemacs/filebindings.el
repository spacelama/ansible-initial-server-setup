;;; cperl-mode is preferred to perl-mode
;;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

;;make f90 mode default
;; No actually - the comments in http://www.math.u-psud.fr/~anm_edp/local/env/fic_env/.emacsf90 say to use normal fortran mode for fixed format code.
;; f90 messes with f77 style comments as well
;(setq auto-mode-alist
;      (append '(("\\.f$"  . f90-mode)
;		("\\.F$" . f90-mode))
;	      auto-mode-alist))
;;;;or
;(delete-item 'auto-mode-alist nil 'assoc "\\.f\\'")
;(delete-item 'auto-mode-alist nil 'assoc "\\.F\\'")

(setq auto-mode-alist (append '(("\\.yml$" . yaml-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.yaml$" . yaml-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.1html$" . html-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.bb$" . html-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.pp$" . puppet-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.p$" . cperl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.gnuplot$" . gnuplot-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ino$" . arduino-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)) auto-mode-alist))
(setq auto-mode-alist (append '((".*/tcs/.*/.*\\.\\(cpp\\|c\\|C\\|h\\)$" . drama-c-mode)) auto-mode-alist))
;(setq auto-mode-alist (append '(("/\\(.article\\|.followup\\|.letter\\)$" . message-mode)) auto-mode-alist))
; message-mode in emacs causes M-q (fill) to screw up paragraphs formatting by indenting stuff and subsequent lines.  don't recall why I had previously discarded mail-mode
(setq auto-mode-alist (append '(("/\\(.article\\|.followup\\|.letter\\)$" . mail-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("/\\(Mail\\|News\\)/" . message-mode)) auto-mode-alist))

;;(if 'auc-latex-mode
;;    (setq auto-mode-alist  (append '(("\\.tex" . auc-latex-mode)) auto-mode-alist)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
;;  (c-set-style "K&R")
  (setq c-indent-level 8)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -8)
  (setq c-argdecl-indent 8)
  (setq c-label-offset -8)
  (setq c-continued-statement-offset 8)
  (setq indent-tabs-mode t)
  (c-set-style "linux-tabs-only")
  (setq tab-width 8))

(defun drama-c-mode ()
  "C mode with adjusted defaults for use with DRAMA."
  (interactive)
  (c-mode)
;;  (c-set-style "K&R")
  (setq c-basic-offset 3)
  (setq c-indent-level 3)
  (setq c-brace-imaginary-offset 0)
  (setq c-brace-offset -3)
  (setq c-argdecl-indent 3)
  (setq c-label-offset -3)
  (setq c-continued-statement-offset 3)
  (setq indent-tabs-mode nil)
  (setq tab-width 3))

;; Make a keybinding: `C-c C-c g'
;(define-key markdown-mode-command-map (kbd "g") #'grip-mode)

;; Or start grip when opening a markdown/org buffer
(add-hook 'markdown-mode-hook #'grip-mode)
(add-hook 'org-mode-hook #'grip-mode)

(try-require 'berry-mode)
;; Associate .be files with berry-mode
;;(add-to-list 'auto-mode-alist '("\\.be\\'" . berry-mode))

