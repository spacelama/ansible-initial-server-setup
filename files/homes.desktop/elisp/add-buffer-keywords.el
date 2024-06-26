(require 'cl)

(defun font-lock-add-buffer-keywords (keywords &optional append)
  "Add highlighting KEYWORDS for the current buffer.
KEYWORDS should be a list; see the variable `font-lock-keywords'.
By default they are added at the beginning of the current highlighting list.
If optional argument APPEND is `set', they are used to replace the current
highlighting list.  If APPEND is any other non-nil value, they are added at the
end of the current highlighting list.

For example:

 (font-lock-add-buffer-keywords
  '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face append)
    (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" . font-lock-keyword-face)))

adds two fontification patterns: one to fontify `FIXME:' words, even in
comments, and the other to fontify `and', `or' and `not' words as keywords.

Note that some modes have specialised support for additional patterns, e.g.,
see the variables `c-font-lock-extra-types', `c++-font-lock-extra-types',
`objc-font-lock-extra-types' and `java-font-lock-extra-types'."
  ;; This is needed to avoid this operation ending up as a no-op (because
  ;; `font-lock-set-defaults' might get called later, and it might decide to
  ;; set `font-lock-keywords' itself, from scratch).  I understand that some
  ;; older FSF Emacs releases don't do this in `font-lock-add-keywords', so
  ;; we do it here -- which can't hurt.
  (font-lock-set-defaults)
  ;; Use a native implementation if one exists
  (if (fboundp 'font-lock-add-keywords)
      (font-lock-add-keywords nil keywords append)
    ;; Otherwise, use this one that was grabbed from FSF Emacs 21's
    ;; `font-lock-add-keywords' and `font-lock-remove-keywords' functions.
    (if (eq append 'set)
        (setq font-lock-keywords keywords)
      ;; Try to remove duplicates
      (setq font-lock-keywords (copy-sequence font-lock-keywords))
      (dolist (kw keywords)
        (setq font-lock-keywords
              (delete kw
                      ;; The keywords might be compiled
                      (delete (font-lock-compile-keyword kw)
                              font-lock-keywords))))
      (let ((old font-lock-keywords))
        (when (eq (car-safe font-lock-keywords) t)
          (pop old))
        (when append
          (cl-rotatef keywords old))
        (setq font-lock-keywords (append keywords old))))))
