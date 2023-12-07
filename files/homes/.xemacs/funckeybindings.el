;; *** Function Keys ***
;; F2 is undo
;(global-set-key [f2] 'undo)
;; F3 is find-file (that's the Open command for you newbies)
;; From the sample.emacs in XEmacs:
;;     Note: it does not currently work to say
;;         (global-set-key 'f3 "\C-x\C-f")
;;     The reason is that macros can't do interactive things properly.
;;     This is an extremely longstanding bug in Emacs. Eventually,
;;     it will be fixed. (Hopefully...)
;(global-set-key [f3] 'find-file)
;; F4 is mark
;(global-set-key [f4] 'set-mark-command)
;; F5 is copy
;(global-set-key [f5] "\M-w")
;; F6 is paste
;(global-set-key [f6] "\C-y")
;; Shift-F4 is pop mark off of stack
;(global-set-key [(shift f4)] (lambda () (interactive) (set-mark-command t)))
;(global-set-key [f16] (lambda () (interactive) (set-mark-command t)))
;; F7 is save-buffer
;(global-set-key [f7] 'save-buffer)
;; F8 is start macro

;; F1 invokes help
(global-set-key [f1] 'help-command)
(global-set-key [f2] 'dired-other-frame)
(global-set-key [(shift f2)] 'dired)
(global-set-key [f14] 'dired)
(global-set-key [f3] 'grep)
;(global-set-key [f4] 'compile)
;(global-set-key [f4] 'remote-compile)
(global-set-key [f4] 'compile)

(global-set-key [f5] 'delete-region)

(global-set-key [f9] 'start-kbd-macro)
(global-set-key [(shift f9)] 'end-kbd-macro)
(global-set-key [f21] 'end-kbd-macro)
(global-set-key [f10] 'call-last-kbd-macro)
(global-set-key [(shift f10)] 'pm-define)
(global-set-key [f22] 'pm-define)

(global-set-key [(control ?|)] 'shell-command-on-buffer)



;;Set up colors for printing, and then back to default
(global-set-key [f11] 'toggle-option)
(global-set-key [f12] 'prepareprettyprint)
(global-set-key [(shift f12)] 'deprepareprettyprint)
(global-set-key [f24] 'deprepareprettyprint)

;(define-key global-map '(alt x) 'execute-extended-command)
;(define-key global-map 'f8 'function-menu)
;(define-key global-map "\C-cl" 'fume-list-functions)
;(define-key global-map "\C-cg" 'fume-prompt-function-goto)
;(define-key global-map '(shift button3) 'mouse-function-menu)
;(define-key global-map '(meta  button1) 'fume-mouse-function-goto)
