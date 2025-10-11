;;; berry-mode.el --- Major mode for editing Berry script files -*- lexical-binding: t; -*-
;;
;; Major mode for the Berry scripting language
;; Provides syntax highlighting, indentation, and file association.

(defvar berry-mode-hook nil)

(defvar berry-keywords
  '("if" "elif" "else" "for" "while" "break" "continue" "return"
    "class" "def" "function" "end" "try" "catch" "throw"
    "switch" "case" "default" "import" "from" "as"))

(defvar berry-constants
  '("true" "false" "nil"))

(defvar berry-builtins
  '("print" "input" "len" "range" "int" "float" "string" "tasmota"))

(defvar berry-font-lock-keywords
  `(
    ;; Keywords
    (,(regexp-opt berry-keywords 'symbols) . font-lock-keyword-face)
    ;; Constants
    (,(regexp-opt berry-constants 'symbols) . font-lock-constant-face)
    ;; Builtins
    (,(regexp-opt berry-builtins 'symbols) . font-lock-builtin-face)
    ;; Function names after def/function
    ("\\<\\(def\\|function\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 2 font-lock-function-name-face)
    ;; Class names after class
    ("\\<class\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-type-face)
    ;; Variables after var
    ("\\<var\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-variable-name-face)
    ;; self.foo highlighting
    ("\\<self\\.\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-variable-name-face)
    ;; Numbers
    ("\\b[0-9]+\\(\\.[0-9]+\\)?\\b" . font-lock-constant-face)
    ;; Strings (safe quoting)
    ("\\\"\\(\\\\.\\|[^\\\"\\]\\)*\\\"" . font-lock-string-face)
    ;; Operators
    ("[+\-*/%=&|!<>]+" . font-lock-builtin-face)
    ))

(defvar berry-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments start with #
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    ;; Parentheses for function calls
    (modify-syntax-entry ?( "(" st)
    (modify-syntax-entry ?) ")" st)
    (modify-syntax-entry ?[ "(" st)
    (modify-syntax-entry ?] ")" st)
    (modify-syntax-entry ?{ "(" st)
    (modify-syntax-entry ?} ")" st)
    st))

(defun berry-indent-line ()
  "Indent current line for Berry Script.

Rules:
- Blank lines inherit indentation from the previous non-blank line.
- A comment line aligns to the column of the previous line's `#` if present.
- A comment immediately after a block-opener is indented into the block.
- Keywords inside strings or comments are ignored when deciding block structure."
  (interactive)
  (let ((offset 2)
        indent
        (case-fold-search nil)) ;; keywords are case-sensitive
    (save-excursion
      (beginning-of-line)
      ;; Helper functions
      (cl-labels
          ((prev-nonblank-pos ()
             (save-excursion
               (when (> (line-number-at-pos) 1)
                 (forward-line -1)
                 (while (and (not (bobp))
                             (looking-at "^[ \t]*$"))
                   (forward-line -1))
                 (if (bobp) nil (line-beginning-position)))))

           (prev-noncomment-pos ()
             (save-excursion
               (let ((p (funcall #'prev-nonblank-pos)))
                 (when p
                   (goto-char p)
                   (while (and (not (bobp))
                               (looking-at "^[ \t]*#"))
                     (forward-line -1))
                   (if (bobp) nil (line-beginning-position))))))

           (comment-column (pos)
             (when pos
               (save-excursion
                 (goto-char pos)
                 (let ((line (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
                   (when (string-match "#" line)
                     (save-excursion
                       (forward-char (match-beginning 0))
                       (current-column)))))))

           (line-has-block-opener-p (pos)
             (when pos
               (save-excursion
                 (goto-char pos)
                 (let* ((line (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                        (re "\\b\\(class\\|def\\|function\\|if\\|elif\\|else\\|for\\|while\\|try\\|switch\\|case\\)\\b"))
                   (when (string-match re line)
                     (let ((match-beg (+ pos (match-beginning 0))))
                       (not (nth 3 (syntax-ppss match-beg))))))))))

        ;; Start main logic
        (if (looking-at "^[ \t]*$")
            ;; Blank line: same indent as previous nonblank
            (let ((p (funcall #'prev-nonblank-pos)))
              (setq indent (if p
                               (save-excursion (goto-char p) (current-indentation))
                             0)))
          ;; Non-blank line
          (let* ((prev-pos (funcall #'prev-nonblank-pos))
                 (prev-nc-pos (funcall #'prev-noncomment-pos))
                 (prev-indent (if prev-pos
                                  (save-excursion (goto-char prev-pos)
                                                  (current-indentation))
                                0))
                 (prev-hash-col (funcall #'comment-column prev-pos))
                 (curr-is-comment (looking-at "^[ \t]*#"))
                 (prev-was-block
                  (or (and prev-nc-pos (funcall #'line-has-block-opener-p prev-nc-pos))
                      (and prev-pos (funcall #'line-has-block-opener-p prev-pos)))))

            (if curr-is-comment
                ;; Handle comment indentation
                (cond
                 ;; Comment right after a block opener
                 ((and prev-pos prev-was-block)
                  (setq indent (+ prev-indent offset)))
                 ;; Previous line had a '#' → align under it
                 (prev-hash-col
                  (setq indent prev-hash-col))
                 ;; Otherwise just align to previous indent
                 (t
                  (setq indent prev-indent)))
              ;; Handle code indentation
              (cond
               ;; Closing keywords dedent
               ((looking-at "^[ \t]*\\(end\\|elif\\|else\\|case\\|default\\|catch\\)\\b")
                (setq indent (max 0 (- prev-indent offset))))
               ;; Inside parentheses: align under opening paren
               ((nth 1 (syntax-ppss))
                (let ((open-pos (nth 1 (syntax-ppss))))
                  (goto-char open-pos)
                  (forward-char 1)
                  (skip-syntax-forward "- ")
                  (setq indent (current-column))))
               ;; Otherwise: same or deeper if previous line opened a block
               (t
                (setq indent prev-indent)
                (when (and prev-nc-pos
                           (funcall #'line-has-block-opener-p prev-nc-pos))
                  (setq indent (+ prev-indent offset))))))))))

    ;; Apply indentation
    (unless indent (setq indent 0))
    (indent-line-to (max 0 indent))
    (when (< (current-column) indent)
      (move-to-column indent))))

;;;###autoload
(define-derived-mode berry-mode prog-mode "Berry"
  "Major mode for editing Berry script files."
  :syntax-table berry-mode-syntax-table
  (setq-local font-lock-defaults '(berry-font-lock-keywords))
  (setq-local indent-line-function 'berry-indent-line))

(provide 'berry-mode)
;;; berry-mode.el ends here
