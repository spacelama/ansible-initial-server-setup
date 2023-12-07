;; -*- Mode: Emacs-Lisp -*-

;(add-to-list 'tramp-multi-connection-function-alist
;             '("sshc" tramp-multi-connect-rlogin "ssh1killkludge %h %u%n"))

(add-to-list 'tramp-methods
             '("rsyncfsh"
               (tramp-connection-function tramp-open-connection-rsh)
               (tramp-login-program "fsh")
               (tramp-copy-program "rsync")
               (tramp-remote-sh "/bin/sh -i")
               (tramp-login-args ("/bin/sh" "-i"))
               (tramp-copy-args ("-e" "fsh"))
               (tramp-copy-keep-date-arg "-t")
               (tramp-password-end-of-line nil)))

(add-to-list 'tramp-methods
             '("sshC"
               (tramp-connection-function tramp-open-connection-rsh)
               (tramp-login-program "ssh")
               (tramp-copy-program nil)
               (tramp-remote-sh "/bin/sh")
               (tramp-login-args ("-C" "-e" "none"))
               (tramp-copy-args nil)
               (tramp-copy-keep-date-arg nil)
               (tramp-password-end-of-line nil)))

(add-to-list 'tramp-methods
             '("fsh"   (tramp-login-program        "fsh")
               (tramp-login-args           (("%h") ("-C") ("-l" "%u") ("sh" "-i")))
               (tramp-remote-sh            "/bin/sh -i")
               (tramp-copy-program         nil)
               (tramp-copy-args            nil)
               (tramp-copy-keep-date       t)
               (tramp-password-end-of-line nil)))
