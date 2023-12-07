(require 'cl)

(defun fold-region (start end)
  (interactive "r")
  (folding-fold-region start end)
;  (folding-hide-current-entry))
;  (folding-shift-out))
  (folding-context-next-action)
  (folding-context-next-action))

(defun folding-key-settings ()
  (interactive)
  (define-key folding-mode-map "\C-c@F" 'fold-region))

(defun c-key-settings ()
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent))

(defun perl-key-settings ()
  (interactive)
  (define-key cperl-mode-map "\C-m" 'newline-and-indent))
;  (define-key cperl-mode-map "DEL" 'delete-char))
;  (local-set-key [delete] 'delete-char))

(defun fortran-key-settings ()
  (interactive)
  (define-key fortran-mode-map "\C-m" 'newline-and-indent)
;  (local-set-key '(meta return) 'fortran-split-line))
  (define-key fortran-mode-map "\M-\C-m" 'fortran-split-line))

(defun f90-key-settings ()
  (interactive)
  (define-key f90-mode-map "\C-m" 'newline-and-indent)
  (define-key f90-mode-map "\M-\C-m" 'f90-break-line))

;; auto wraparound
;;(defun tex-mode-hook-fn ()
;;   (auto-fill-mode 1))
;;(add-hook 'tex-mode-hook 'tex-mode-hook-fn)
;;(setq tex-mode-hook 'tex-mode-hook-fn)
;;(add-hook 'latex-mode-hook 'tex-mode-hook-fn)

(defun emacs-lisp-key-settings ()
  (interactive)
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent))


(add-hook 'folding-mode-hook 'folding-key-settings)
;(setq folding-default-keys-function 'folding-key-settings)

(add-hook 'c-mode-common-hook 'c-key-settings)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-key-settings)
(add-hook 'cperl-mode-hook  'perl-key-settings)
(add-hook 'fortran-mode-hook 'fortran-key-settings)
(add-hook 'f90-mode-hook 'f90-key-settings)

(defun suspend-only-term ()
  (interactive)
;  (if (controlling-tty-p)
;      (suspend-emacs)))
  (suspend-tty)
  )

;;disable the ctrl c ctrl z combination that minimises all windows - very very annoying
;;(global-set-key "\C-x\C-z" nil)
(global-unset-key "\C-x\C-z")
;(global-unset-key "\C-z")
(global-set-key "\C-z" 'suspend-only-term)
;(global-set-key "\C-z" 'suspend-tty)

;; vvb-set-columns - useless and accidentally triggerable
;(global-unset-key "\C-xc")

;(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-x\C-b" 'ibuffer)
;(global-set-key "\C-x\C-c" 'delete-frame) ;;this is because we are using gnuclient now - old habits die hard. To really kill xemacs, call M-x kill-emacs

(defun doublecheck-kill ()
  (interactive)
  (if (save-some-buffers-dont-exit)
      (progn
        (ding)
        (if (y-or-n-p "really quit Emacs?")
            (progn
              (ding)
              (if (y-or-n-p "really *really* quit Emacs (If you are not TimC, then the only acceptable answer is 'n')?")
                  (save-buffers-kill-emacs))))))
  (message "Thank you, come again"))

;(global-set-key "\C-x\C-c" 'doublecheck-kill)

(XEmacs
 (global-set-key 'button4 'scroll-down)
 (global-set-key 'button5 'scroll-up)
 )

(GNUEmacs
 ;; Stop at the end of the file, not just add lines
 (setq next-line-add-newlines nil)

 (defun backward-other-window ()
   "Go to previous window"
   (interactive)
   (other-window -1))
 (global-set-key [C-tab] 'other-window)
 (global-set-key [C-S-iso-lefttab] 'backward-other-window)
 )

(define-key global-map "\M-g" 'goto-line)

(define-key global-map "\C-cg" 'go-matching-paren)
(define-key global-map "\C-cs" 'show-matching-paren)

(define-key global-map "\M-[1;5C" 'forward-word)    ;; is set in inputrc, but doesn't seem to work?
(define-key global-map "\M-[1;5D" 'backward-word)   ;; is set in inputrc, but doesn't seem to work?
(define-key global-map "\M-[1;5H" 'beginning-of-buffer)
(define-key global-map "\M-[1;5F" 'end-of-buffer)
(define-key global-map "\M-[1;5A" 'backward-block-of-lines)
(define-key global-map "\M-[1;5B" 'forward-block-of-lines)

(load-library "f90")
;;we don't want the (sit-for 1)

(defun f90-match-end ()
  "From an end foo statement, find the corresponding foo including name."
  (interactive)
  (let ((count 1) (top-of-window (window-start)) (matching-beg nil)
        (end-point (point)) (case-fold-search t)
        beg-name end-name beg-block end-block end-struct)
    (if (save-excursion (beginning-of-line) (skip-chars-forward " \t0-9")
                        (setq end-struct (f90-looking-at-program-block-end)))
        (progn
          (setq end-block (car end-struct))
          (setq end-name  (car (cdr end-struct)))
          (save-excursion
            (beginning-of-line)
            (while 
                (and (not (zerop count))
                     (let ((stop nil) notexist)
                       (while (not stop)
                         (setq notexist
                               (not (re-search-backward 
                                     (concat "\\(" f90-blocks-re "\\)") nil t)))
                         (if notexist
                             (setq stop t)
                           (setq stop
                                 (not (or (f90-in-string)
                                          (f90-in-comment))))))
                       (not notexist)))
              (beginning-of-line) (skip-chars-forward " \t0-9")
              (cond ((setq matching-beg
                           (cond
                            ((f90-looking-at-do))
                            ((f90-looking-at-if-then))
                            ((f90-looking-at-where-or-forall))
                            ((f90-looking-at-select-case))
                            ((f90-looking-at-type-like))
                            ((f90-looking-at-program-block-start))))
                     (setq count (- count 1)))
                    ((looking-at (concat "end[ \t]*" f90-blocks-re "\\b"))
                     (setq count (+ count 1)))))
            (if (not (zerop count))
                (message "No matching beginning.")
              (f90-update-line)
              (if (eq f90-smart-end 'blink)
                  (if (< (point) top-of-window)
                      (message "Matches %s: %s"
                               (what-line)
                               (buffer-substring
                                (progn (beginning-of-line) (point))
                                (progn (end-of-line) (point))))
                    (sit-for 0.1)))
              (setq beg-block (car matching-beg))
              (setq beg-name (car (cdr matching-beg)))
              (goto-char end-point)
              (beginning-of-line)
              (f90-block-match beg-block beg-name end-block end-name)))))))


;; enter Ctrl-\ rot13 to get into rot13 input mode, and use M-x
;; rot13-other-window to see the display.  To change input modes
;; later, can reset rot13 as default using Ctrl-u Ctrl-\

;; In emacs, rot13-region seems to be defined by rot13.el too
;emacs compatibility:
(unless (fboundp 'characterp) (defalias 'characterp 'integerp))
(unless (fboundp 'int-to-char) (defalias 'int-to-char 'identity))
(unless (fboundp 'char-to-int) (defalias 'char-to-int 'identity))
(when (try-require 'quail)
  (quail-define-package
   "rot13" "rot13" "r13" nil
   "ROT-13 input method.
Alphabetic characters are moved 13 places in the alphabet on input.  ")

  (let ((i -1)
	(a (char-to-int ?a))
	(A (char-to-int ?A)))
    (while (< (cl-incf i) 26)
      (quail-defrule (string (+ i a)) (char-to-string (int-to-char (+ (%
								       (+ i 13) 26) a))))
      (quail-defrule (string (+ i A)) (char-to-string (int-to-char (+ (%
								       (+ i 13) 26) A)))))))



; original definition of forward-paragraph is in paragraphs.el
; (backward-paragraph just calls forward-paragraph -1)
(defun forward-block (&optional backward)
  "Move cursor forward to next occurrence brace, bracket or
 paragraph, depending on major-mode In text based major modes,
 this is similar to `forward-paragraph'."
  (interactive)

  (setq forwparasearch "\n[[:blank:]]*\n")
  (setq backparasearch "\n[[:blank:]]*\n")

  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (setq forwparasearch (concat "\\(" forwparasearch "\\|)[[:blank:]]*)\\)"))
    (setq backparasearch (concat "\\(" backparasearch "\\|)[[:blank:]]*)\\)"))
    )
   ((eq major-mode 'sh-mode)
    (setq forwparasearch (concat "\\(" forwparasearch "\\|\\<\\(case\\|if\\|elif\\|else\\|while\\)\\>\\|;;\\)"))
    (setq backparasearch (concat "\\(" backparasearch "\\|\\<\\(esac\\|elif\\|else\\|fi\\|done\\)\\>\\|;;\\)"))
    )
   ((or
     (eq major-mode 'c-mode)
     (eq major-mode 'cperl-mode)
     )
    (setq forwparasearch (concat "\\(" forwparasearch "\\|}\\)"))
    (setq backparasearch (concat "\\(" backparasearch "\\|{\\)"))
    )
   ((eq major-mode 'mail-mode)
    (setq forwparasearch "\n>*[[:blank:]]*\n")
    (setq backparasearch "\n>*[[:blank:]]*\n")
    )
   (t
    (setq forwparasearch "\n[[:blank:]]*\n")
    (setq backparasearch "\n[[:blank:]]*\n")
    )
   )

 
  (cond
   ((eq backward nil)
                                        ;  (skip-chars-forward parasearch)
    (skip-chars-forward " \t\n")
    (when (not (search-forward-regexp forwparasearch nil t))
      (goto-char (point-max)) ))
   (t
    (skip-chars-backward " \t\n")
    (when (not (search-backward-regexp backparasearch nil t))
      (goto-char (point-min)) ))
   )
  )


(defun backward-block () 
  "Move cursor backward to previous occurrence of double newline char. 
 See: `forward-block'"
  (interactive)
  (forward-block t)
  )

(define-key global-map (kbd "C-<up>") 'backward-block)
(define-key global-map (kbd "C-<down>") 'forward-block)

;; https://emacs.stackexchange.com/questions/9322/how-can-i-quit-ediff-immediately-without-having-to-type-y
(add-hook 'ediff-startup-hook
          (lambda ()
            (local-set-key (kbd"q") 'my-ediff-quit)))

(defun my-ediff-quit ()
  "If any of the ediff buffers have been modified, ask if changes
should be saved. Then quit ediff normally, without asking for
confirmation"
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let* ((buf-a ediff-buffer-A)
         (buf-b ediff-buffer-B)
         (buf-c ediff-buffer-C)
         (ctl-buf (current-buffer))
         (modified (remove-if-not 'buffer-modified-p
                                  (list buf-a buf-b buf-c))))
    ;; (let ((save (if modified (yes-or-no-p "Save changes?")nil)))
    ;;   (loop for buf in modified do
    ;;         (progn
    ;;           (set-buffer buf)
    ;;           (if save
    ;;               (progn
    ;;                 (save-buffer)
    ;;                 (set-buffer-modified-p nil)))))
      (set-buffer ctl-buf)
      (ediff-really-quit nil)));)
