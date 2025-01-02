
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defmacro GNUEmacs (&rest x)
  (list 'if (string-match "GNU Emacs [0-9]" (version)) (cons 'progn x)))
(defmacro XEmacs (&rest x)
  (list 'if (string-match "XEmacs\\|Lucid" (version)) (cons 'progn x)))
(defmacro GNULinux (&rest x)
  (list 'if (string-match "linux" (prin1-to-string system-type)) (cons 'progn x)))
(defmacro Windows (&rest x)
  (list 'if (string-match "windows" (prin1-to-string system-type)) (cons 'progn x)))


; WARNING: This file doesn't seem to be executed by modern xemacs
; anymore.  Make sure it is the bare minimum and put anything
; common in a common file sourced by both it and gnu-emacs

(XEmacs
    ;;; XEmacs backwards compatibility file
 (setq user-init-file "~/.xemacs/init.el")
 (setq custom-file "~/.xemacs/custom.el")
 
 (load-file user-init-file)
 (load-file custom-file))

(GNUEmacs
  ;;; load ~/.gnu-emacs or, if not exists /etc/skel/.gnu-emacs
  ;;; For a description and the settings see /etc/skel/.gnu-emacs
  ;;;   ... for your private ~/.gnu-emacs your are on your one.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   ;; Custom Settings
   ;; ===============
   ;; To avoid any trouble with the customization system of GNU emacs
   ;; we set the default file ~/.gnu-emacs-custom
   (setq custom-file "~/.gnu-emacs-custom")
   (load "~/.gnu-emacs-custom" t t)

   (if (file-readable-p "~/.gnu-emacs")
        (load "~/.gnu-emacs" nil t)))

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(fancy-splash-delay 1)
;;  '(fancy-splash-max-time 1))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  )
