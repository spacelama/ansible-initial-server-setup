;; trying to work around a bug in debian emacs 29 where sort now seems
;; to be regarding any buffer that has been loaded (at session
;; startup) but never visited, as newer than all other visited files
;; Indeed! The below functions reveal that those buffers have dates in
;; the future from a few years to 2115-10-22!

;; https://emacs.stackexchange.com/questions/31786/can-ibuffer-show-buffers-last-viewing-time

;; ah, buffer-display-time has become corrupted somehow in
;; emacs.desktop, so desktop.el munges it up

(try-require 'ibuffer)

(define-ibuffer-column last-viewed
  (:name "Last-viewed" :inline t)
  (with-current-buffer buffer
    (format-time-string "%Y-%m-%d %R" (or buffer-display-time 0))))

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (last-viewed 18 -1 :left)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;; (define-ibuffer-sorter last-viewed
;;   "Sort the buffers by last viewed time."
;;   (:description "last-viewed")
;;   (string-lessp (with-current-buffer (car a)
;;        (format-time-string "%Y-%m-%d %R" buffer-display-time))
;;      (with-current-buffer (car b)
;;        (format-time-string "%Y-%m-%d %R" buffer-display-time))))
;; (define-key ibuffer-mode-map (kbd "s v") 'ibuffer-do-sort-by-last-viewed)
