; Subject: Re: This is what happens when your cat uses your Emacs 
; Date: Sun, 15 Jan 2012 15:15:54 -0700
; From: Eric Schulte <eric.schulte@gmx.com>
; To: Stefan Monnier <monnier@iro.umontreal.ca>
; Cc: emacs-devel@gnu.org, Jose E. Marchesi <jemarch@gnu.org>

(defun cat-command ()
  "A command for cats."
  (interactive)
  (require 'animate)
  (let ((mouse "
           ___00
        ~~/____'>
          \"  \"")
        (h-pos (floor (/ (window-height) 2)))
        (contents (buffer-string))
        (mouse-buffer (generate-new-buffer "*mouse*")))
    (save-excursion
      (switch-to-buffer mouse-buffer)
      (insert contents)
      (setq truncate-lines t)
      (animate-string mouse h-pos 0)
      (dotimes (_ (window-width))
        (sit-for 0.01)
        (dotimes (n 3)
          (goto-line (+ h-pos n 2))
          (move-to-column 0)
          (insert " "))))
    (kill-buffer mouse-buffer)))


(define-key global-map (kbd "C-M-S-s-z") 'cat-command)
