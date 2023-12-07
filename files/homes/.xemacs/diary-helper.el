(defun diary-output-entries (&optional ndays)
  "Stolen from diary-lib.el:diary-mail-entries modified to write-file ~/.diaryoutput and exit-calendar instead of invoking MTA"
  (interactive "P")
  (let ((diary-display-function 'diary-fancy-display))
    (diary-list-entries (calendar-current-date) (or ndays diary-mail-days)))
    (compose-mail diary-mail-addr
                  (concat "Diary entries generated "
                          (calendar-date-string (calendar-current-date))))
    (insert
     (if (get-buffer diary-fancy-buffer)
         (with-current-buffer diary-fancy-buffer (buffer-string))
       "No entries found"))
    (write-file "~/.diaryoutput")
    (exit-calendar)
    
)


;;---great bit of code from Jeff Miller to highlight appointments in red on modeline---
;(defface appt-face
;  '((t (:foreground "red" :background "white")))
;  "Face to indicate a current appointment."
;  :group 'appt)

;(defadvice appt-disp-window (before appt-hilite-more activate)
;  (when appt-mode-line-string
;    (put-text-property 1 (- (length appt-mode-line-string) 1)
;		       'face 'appt-face appt-mode-line-string)))

;(defadvice appt-check (after appt-hilite activate)
;  (when appt-mode-line-string
;    (put-text-property 1 (- (length appt-mode-line-string) 1)
;		       'face 'appt-face appt-mode-line-string)
;    (force-mode-line-update)))

;;---------------------------------

