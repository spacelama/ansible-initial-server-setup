; http://stackoverflow.com/questions/4477376/some-emacs-desktop-save-questions-how-to-change-it-to-save-in-emacs-d-emacs
;; Automatically save and restore sessions

(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)

;;If already making use of a desktop (through xemacs -f desktop-read), save it when buffers are updated.
(defun save-desktop ()
  (interactive)
  (if desktop-dirname
      (progn
        ;;      (ding t)))
        (frame-configuration-to-register ?F)
        (desktop-save desktop-dirname)
        (message "We're saving session as asked by save-desktop")
        (session-save-session)
        (message (concat "saved desktop to " desktop-dirname ".emacs.desktop and session to .session")))
    (message "use (desktop-save) to specify a location or (load-desktop) before calling (save-desktop)")
    nil));;last line is to make sure it succeeds so we can go further!

(defun load-desktop ()
  ;;loads the desktop, but doesn't save the desktop after each file
  ;;load whilst actually restoring the desktop - only saves once you
  ;;open a file manually thereafter
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (message "We're loading desktop and session as asked by load-desktop")
;    (remove-hook 'find-file-hooks 'save-desktop)
    (setq tmpfailed nil)
    (condition-case err
        (progn
          (session-initialize)
          (desktop-read))
      (error
       (beep)
       (message "Error? Not enabling desktop-save")
       (setq tmpfailed t)))
    (if tmpfailed
        nil
      (progn
        (desktop-save-mode 1)
        ;(add-hook 'find-file-hooks 'save-desktop t)
;      (jump-to-register ?F)
        (message "Successfully loaded desktop from ~")
        ))))

;;I really don't know when the best place to save a desktop is - the following seems to work
; You don't want to save when loading files, because if a desktop-load
; fails, then it will try to save the incomplete contents! Perhaps I
; can just remove the hook when loading a desktop, and add it back
; when it finishes? If it aint broke though, don't fix it!
;(add-hook 'after-set-visited-file-name-hooks 'save-desktop)
;(add-hook 'write-contents-hooks 'save-desktop)

(defun initialise-desktop-session ()
    (message "running initialise-desktop-session")
    (message "*** Setting up session ***")
;    (when (try-require 'desktop)
     (when (try-require 'session)
      (setq desktop-globals-to-save '(desktop-missing-file-warning)))
;;;(when (try-require 'desktop)
;;;  (desktop-load-default)
;;;  (try-require "desktop-phase")) ;; <--- has to be loaded before the 
;;                               ;;      buffers are created...

;(add-hook 'after-init-hook (lambda ()
;                             (session-initialize)
;                             (desktop-load-default)
;                             (desktop-read)))

;
;(when (try-require 'revive)
;  (load "~/.xemacs/savefilesrevive.el"))   ;only do after succesfully loaded desktop


    (message "*** Setting up desktop ***")

    (when (try-require 'desktop)
                                        ;(add-hook 'find-file-hooks 'save-desktop t)
      (load-desktop)
      ))

;(add-hook 'after-init-hook 'initialise-desktop-session)   ;only do after succesfully loaded desktop
; https://emacs.stackexchange.com/questions/12351/when-to-call-find-font-if-launching-emacs-in-daemon-mode

;; Below macro is used to wrap stuff that need to be run only after emacs
;; starts up completely. This is very crucial when calling functions like
;; `find-font' which return correct value only after emacs startup is finished
;; especially when emacs is started in daemon mode.
(defmacro do-once-1-sec-after-emacs-startup (&rest body)
  `(run-with-idle-timer 1 ; run this after emacs is idle for 1 second
                        nil ; do this just once; don't repeat
                        (lambda () ,@body)))
;; Load the desktop just once after a second delay after
;; emacs startup. This trick works when emacs is launched in regular or daemon
;; mode
(do-once-1-sec-after-emacs-startup
                       (initialise-desktop-session))

;(defun setup-session-after-frame-load ()
;  "Remove self from `focus-in-hook' (need to run this just once), then load session."
;  (remove-hook 'focus-in-hook 'setup-session-after-frame-load)
;  (initialise-desktop-session))
;; For non-daemon, regular emacs launches, the frame/fonts are loaded *before*
;; the emacs config is read. But when emacs is launched as a daemon (using
;; emacsclient, the fonts are not actually loaded until the point when the
;; `after-make-frame-functions' hook is run. But even at that point, the frame
;; is not yet selected (for the daemon case). Without a selected frame, the
;; `find-font' will not work correctly. So we do the font check in
;; `focus-in-hook' instead by which all the below are true:
;;  - Fonts are loaded (in both daemon and non-daemon cases).
;;  - The frame is selected and so `find-font' calls work correctly.
;(add-hook 'focus-in-hook 'setup-session-after-frame-load)

;(add-hook 'kill-buffer-hook 'save-desktop)

;;(defun do-you-really-want-to-save-buffers-kill-emacs ()
;;  "avoids premature ejection"
;;  (interactive)
;;  (if (yes-or-no-p "Really? ")
;;      (save-buffers-kill-emacs)
;;    (message "stop jerking me around")))
;;(global-set-key "\C-x\C-c"
;;'do-you-really-want-to-save-buffers-kill-emacs)

(defun check-vc-checkin ()
  "checks whether the file is registered with VC, every time it is saved"
  (message "Checking whther file is registered for VC")
  (if (vc-registered (buffer-file-name))
      (auto-vc-checkin)
    (message "Not registered with VC. Not checking in and out.")))

(defun auto-vc-checkin ()
  "checks in the file if it is registered"
  (message "Is registered. Checking in.")
  (vc-checkin (buffer-file-name) (vc-workfile-version (buffer-file-name)) "autocheckin");;dont increment the version number when utomatically checking in.
;;  (vc-next-action t)
  (message "Checked in. Checking out again.")
;;  (vc-next-action t))
  (vc-checkout-writable-buffer))

;;(add-hook 'after-save-hook 'check-vc-checkin)


(defun timc-diary-save-hook ()
"Stuff to do when saving the diary files."
  (when (string-match "^diary$" (buffer-name))
;    (appt-initialize)
    (if (fboundp 'show-all-diary-entries)
        (show-all-diary-entries))
    (message "all diary entries displayed")))

(add-hook 'after-save-hook 'timc-diary-save-hook)





(defun timc-html-save-hook ()
  "Do personal setting of file permissions"
;  (let ((permission #O644))
  (let ((permission 420))   ;decimal
    (when (string-match ".*\\.\\.*html$" buffer-file-name)
      (message "set permissions of %s to %o" buffer-file-name permission)
      (set-file-modes buffer-file-name permission)))
  nil)

(add-hook 'after-save-hook 'timc-html-save-hook)

(defconst host-name
  (or (getenv "HOSTNAME") "localhost"))

(defun timc-crontab-save-hook ()
  "Stuff to do when saving crontab files."
  (when (string-match "^/tmp/crontab.*$" (buffer-file-name))
    (let ((filename (concat "~/crontab-" host-name)))
      (copy-file buffer-file-name filename t)
      (message "crontab saved to %s too" filename))))

(add-hook 'after-save-hook 'timc-crontab-save-hook)

(defun timc-copy-written-files-to-multiple-places ()
  "scans ~/.emacs.d/copy-files.list for source files to copy to multiple destination files upon the source file being saved"
  (interactive)

  (let ((source-buffer-file-name (buffer-file-name)))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/copy-files.list")
      (while (re-search-forward (concat "^\\(" source-buffer-file-name "\\) \\(.*\\)" ) nil t)
        (let ((dest-buffer-file-name (match-string-no-properties 2)))
          (message "copying %s->%s after write" source-buffer-file-name dest-buffer-file-name)
          (copy-file source-buffer-file-name dest-buffer-file-name t t t t)
          (sit-for 0.2)
          (message "finished copying %s->%s after write" source-buffer-file-name dest-buffer-file-name)
          ))
      ;;          (error (concat "Search failed for: " source-buffer-file-name))
      ))
  )

(add-hook 'after-save-hook 'timc-copy-written-files-to-multiple-places)

; https://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave
(defun hlu-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
		 (not (file-executable-p buffer-file-name)))
	(set-file-modes buffer-file-name
			(logior (file-modes buffer-file-name) #o111))
	(message (concat "Made " buffer-file-name " executable"))))))

(add-hook 'after-save-hook 'hlu-make-script-executable)
