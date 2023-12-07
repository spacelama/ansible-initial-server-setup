;; -*- coding:utf-8 mode: emacs-lisp -*-

; http://www.suse.de/~mfabian/suse-cjk/init-i18n.el



;; $Id: init-i18n.el,v 1.2 2008/09/12 15:03:02 tconnors Exp $ 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set some useful defaults for CJK:
;;
;; tested with "GNU Emacs 21.2.1" and 
;;             "XEmacs 21.4 (patch 12) Portable Code"
;; might not work with older Emacs versions.
;;
;; Fri Feb 28 11:21:25 2003  Mike FABIAN  <mfabian@suse.de>

(when (locate-library "latin-unity") ;; exists only in XEmacs
  (require 'latin-unity))
(when (locate-library "latin-euro-input") ;; exists only in XEmacs
  (require 'latin-euro-input))
;; require Mule-UCS:
;; Mule-UCS is already included in XEmacs >= 21.4.6.
;; For older versions of XEmacs please install mule-ucs-xemacs.rpm
;; For Emacs please install mule-ucs.rpm
;;
;; Note that Mule-UCS *must* be initialized before bbdb!
;; (see Message-ID: <87elvluefv.fsf@cachemir.echo-net.net> in nnml:ding)
;; From: Roland Mas <lolando@debian.org>
;; Date: 26 Mar 2001 10:23:00 +0200
(when (locate-library "un-define")
  (require 'un-define)
  (require 'unicode)
  ;; requiring unidata is optional:
  (require 'unidata)
  (setq unidata-default-file-name "~/UnicodeData-Latest.txt"))

;; Language environments

(when (and (getenv "LANG")
	   (string-match "ja" (getenv "LANG")))
  (set-language-environment "Japanese"))

(when (and (getenv "LANG")
	   (string-match "ko" (getenv "LANG")))
  (set-language-environment "Korean"))

;; some settings for coding systems are not yet correct
;; after 'set-language-environment', especially not in UTF-8 locales, fix this:

(let* ((tmp (shell-command-to-string "locale charmap"))
       (tmp (substring tmp 0 (string-match "\[ \t\n\]" tmp)))
       (tmp (downcase tmp)))
  (when (or (and (fboundp 'find-coding-system) ; XEmacs only
		 (find-coding-system (intern tmp)))
	    ;; Emacs:
	    (coding-system-p (intern tmp)))
    ;; set the coding system priorities:
    ;; (this is also important to make XIM in utf-8 work for XEmacs
    ;; because XEmacs has no variable/function to set the
    ;; coding-system for XIM, it is just autodetected which
    ;; will work correctly only when the coding-system priorities
    ;; are OK.)
    (if (not (and (string-match "XEmacs" emacs-version)
		  (string-match "utf-8" tmp)))
	(prefer-coding-system (intern tmp))
      ;; it's strange, but (prefer-coding-system 'utf-8) doesn't
      ;; work in XEmacs.
      ;; use 'set-coding-priority-list instead, which achieves
      ;; the same and works for 'utf-8 as well:
      (set-coding-priority-list (list (intern tmp)))
      (set-coding-category-system (intern tmp) (intern tmp)))
    (set-default-coding-systems (intern tmp))
    (set-keyboard-coding-system (intern tmp))
    (set-terminal-coding-system (intern tmp))
    (when (and (string-match "XEmacs" emacs-version) 
	       (locate-library "lookup")
	       (string-match "utf-8" tmp))
      ;; It seems that lookup must loaded before the
      ;; lookup-process-coding-system can be changed.
;      (require 'lookup)
      (setq lookup-process-coding-system 'utf-8))))

(unless (terminal-coding-system)
  ;; should actually never happen, if 'terminal-coding-system' is still not
  ;; set here, something must have gone wrong:
  (set-terminal-coding-system 'iso-8859-15))

(unless (string-match "XEmacs" emacs-version)
  ;; only Emacs has `selection-coding-system'. The default, `compound-text' is
  ;; usually good, it works for pasting from Emacs into most applications
  ;; e.g. kterm, netscape, xyaku ...
  ;; But if you want to paste from Emacs into rxvt, you may
  ;; have to set this to something else, for example `euc-jp in ja_JP.eucJP locale.
  (set-selection-coding-system 'compound-text-with-extensions))

;;----------------------------------------------------------------------
;; My personal settings for file-coding-system-alist
;; Depends very much on personal preferences and what files are
;; typically edited. Edit this as needed!
;; 
(when (string-match "UTF-8" (shell-command-to-string "locale charmap"))
  (if (string-match "XEmacs" emacs-version)
      (setq file-coding-system-alist
	    (append (list
		     '("\\.patch" . binary)
		     '("\\.dif+" .binary)
		     '("\\.changes" . binary)
		     '("\\.ycp" . iso-8859-1)
		     '("putonftp" . iso-8859-1)
		     '("^edit_patchinfo.*" . iso-8859-1)
		     '("\\.bbdb" . iso-2022-8)
		     '("\\.spec" . utf-8)
		     '("\\.po" . utf-8)
		     ;; I would like to open most files in utf-8 mode by default,
		     ;; unfortunately this is not usable for XEmacs because
		     ;; file-coding-system-alist overrides
		     ;; -*- coding: foo -*- cookies in files
		     ;;
		     ;; and all the rest is utf-8:
					;'("" . utf-8)
		     )
		    file-coding-system-alist))
    (setq file-coding-system-alist
	  (append (list
		   '("\\.patch" . binary)
		   '("\\.dif+" .binary)
		   '("\\.changes" . binary)
		   '("\\.ycp" . iso-8859-1)
		   '("putonftp" . iso-8859-1)
		   '("\\.bbdb" . iso-2022-8)
		   ;; Unfortunately GNU Emacs runs into some problems as well when
		   ;; trying to open all files in utf-8 mode by default.
		   ;; 
		   ;; and all the rest is utf-8:
					; Problem with japanese-egg-canna in Emacs?
					; '("" . utf-8)
		   )
		  file-coding-system-alist)))
  )

;;----------------------------------------------------------------------
;; The above language environment and encoding setup causes problems
;; when reading non-Japanese Mail with Gnus under XEmacs!!!
;; (Gnus in Emacs doesn't seem to have this problem. 
;; I'm currently using Gnus v5.8.8)
;;
;; The default-buffer-file-coding-system must be set back to 
;; something more reasonable:
;;
;; XEmacs will use iso-2022-jp as the default-buffer-file-coding-system
;; when started with LANG=ja_JP or when
;;    (set-language-environment "Japanese")
;; has been used.
;; This will cause conversion of 8-bit German e-mails to iso-2022-jp, which
;; is nonsense.
;; Then,
;;    (set-default-coding-systems 'euc-jp) 
;; will make XEmacs use euc-jp as the default-buffer-file-coding-system
;; which is also nonsense and destroys German mails.
;; 
;; Using iso-2022-8 as the default-buffer-file-coding system
;; is a much better choice, Gnus then works correctly for mails in all
;; languages (tested for Japanese, Korean, Chinese, German, Czech ...)

(when (string-match "XEmacs" emacs-version)
  (set-default-buffer-file-coding-system 'iso-2022-8))

;;----------------------------------------------------------------------
;; bbdb:

(setq file-coding-system-alist
            (append (list
		            '("\\.bbdb" . iso-2022-8))
		          file-coding-system-alist))

;;----------------------------------------------------------------------
;; Manpage related stuff:

;; don't use a complicated pager like "less", it can only make things worse
(setenv "PAGER" "cat") ; indeed necessary for Emacs in combination with jgroff!!!

;; make it possible to switch back to English manpages, even
;; when XEmacs was started with "LANG=ja_JP" (enter "C" or "en_US" for the locale)

(defun set-man-locale (arg)
  (interactive "sLocale for manual entries: ")
  (setq Manual-switches (list (concat "-L" arg))))

;; I have currently no idea why Japanese man-pages don't display
;; correctly in GNU Emacs when running in ja_JP.UTF-8 locale
;;     (modify-coding-system-alist 'process "man" '(utf-8 . utf-8))
;; apparently doesn't help.
;; In XEmacs, it works already after setting the coding system priorities
;; correctly. Why not in GNU Emacs?

;;----------------------------------------------------------------------
;; Input method stuff:
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; For old Mule 2.3: (not necessary for XEmacs, jserver-host-name
;; doesn't even exist in XEmacs)
;; For Japanese input using wnn4  the name of the jserver-host must
;; be set:

(if (boundp 'jserver-host-name)
    (set-jserver-host-name "localhost"))

;;----------------------------------------------------------------------
;; Canna:

(when (string-match "XEmacs" emacs-version)
  (setq canna-use-color t)
  (setq default-input-method 'japanese-canna))

;;----------------------------------------------------------------------
;; SKK:

;;(unless (string-match "XEmacs" emacs-version)
;;  (require 'skk-autoloads))

;; suggested bindings:
;; (global-set-key "\C-x\C-j" 'skk-mode) ;; conflict with `dired-jump-back'
(global-set-key [(control c) k] 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
;; (global-set-key "\C-xt" 'skk-tutorial)

(setq skk-large-jisyo "/usr/share/xemacs21/mule-packages/etc/skk/SKK-JISYO.L")

(add-hook 'isearch-mode-hook
	  (function (lambda ()
		      (and (boundp 'skk-mode) skk-mode
			   (skk-isearch-mode-setup)))))

(add-hook 'isearch-mode-end-hook
     	  (function
     	   (lambda ()
     	     (and (boundp 'skk-mode) skk-mode (skk-isearch-mode-cleanup))
     	     (and (boundp 'skk-mode-invoked) skk-mode-invoked
     		  (skk-set-cursor-properly)))))

(setq skk-use-color-cursor t)
(setq skk-report-set-cursor-error t)
;;(setq skk-status-indicator 'left)
(setq skk-status-indicator nil)

;;(setq skk-server-host "localhost") 
;;(setq skk-server-host nil)
;;(setq skk-server-prog "/your/path/to/skkserv")
;;(setq skk-server-jisyo "/your/path/to/SKK-JISYO.L")

;;----------------------------------------------------------------------
;; international printing with ps-mule.el:
;; (see /usr/share/xemacs/xemacs-packages/lisp/ps-print/ps-mule.el)

;;  `non-latin-printer'     This is the value to use when you have a japanese
;;			    or korean PostScript printer and want to print
;;			    buffer with ASCII, Latin-1, Japanese (JISX0208 and
;;			    JISX0201-Kana) and Korean characters.  At present,
;;			    it was not tested the Korean characters printing.
;;			    If you have a korean PostScript printer, please,
;;			    test it.
;;
;;  `bdf-font'              This is the value to use when you want to print
;;			    buffer with BDF fonts.  BDF fonts include both latin
;;			    and non-latin fonts.  BDF (Bitmap Distribution
;;			    Format) is a format used for distributing X's font
;;			    source file.  BDF fonts are included in
;;			    `intlfonts-1.2' which is a collection of X11 fonts
;;			    for all characters supported by Emacs.  In order to
;;			    use this value, be sure to have installed
;;			    `intlfonts-1.2' and set the variable
;;			    `bdf-directory-list' appropriately (see ps-bdf.el
;;			    for documentation of this variable).
;;
;;  `bdf-font-except-latin' This is like `bdf-font' except that it is used
;;			    PostScript default fonts to print ASCII and Latin-1
;;			    characters.  This is convenient when you want or
;;			    need to use both latin and non-latin characters on
;;			    the same buffer.  See `ps-font-family',
;;			    `ps-header-font-family' and `ps-font-info-database'.

(setq ps-multibyte-buffer 'non-latin-printer)
