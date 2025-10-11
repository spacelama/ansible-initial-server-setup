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
  "Indent current line for Berry Script, handling blank lines and comments gracefully."
  (interactive)
  (let ((indent 0)
        (offset 2)
        (ppss (syntax-ppss))
        prev-indent
        prev-code
        (case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      ;; Case 1: current line is a comment -> align with previous comment if any
      (if (looking-at "^[ \t]*#")
          (progn
            (save-excursion
              (forward-line -1)
              ;; Skip blank lines backward
              (while (and (not (bobp))
                          (looking-at "^[ \t]*$"))
                (forward-line -1))
              (cond
               ;; Previous line also a comment → align to it
               ((looking-at "^[ \t]*#")
                (setq indent (current-indentation)))
               ;; Otherwise align to previous non-empty line
               (t (setq indent (current-indentation))))))
        ;; Case 2: closing keywords
        (cond
         ((looking-at "^[ \t]*\\(end\\|elif\\|else\\|case\\|default\\|catch\\)\\b")
          (save-excursion
            (forward-line -1)
            ;; Skip blank lines backward
            (while (and (not (bobp))
                        (looking-at "^[ \t]*$"))
              (forward-line -1))
            (setq indent (max 0 (- (current-indentation) offset)))))
         ;; Case 3: inside parentheses → align under opening
         ((nth 1 ppss)
          (goto-char (nth 1 ppss))
          (forward-char 1)
          (skip-syntax-forward "- ")
          (setq indent (current-column)))
         ;; Case 4: normal indentation logic
         (t
          (save-excursion
            (forward-line -1)
            ;; Skip blank lines backward
            (while (and (not (bobp))
                        (looking-at "^[ \t]*$"))
              (forward-line -1))
            (setq prev-indent (current-indentation))
            ;; Strip comments from previous line before testing for block keywords
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (setq prev-code (car (split-string line "#" t)))
              (setq indent prev-indent)
              (when (and prev-code
                         (string-match
                          ".*\\b\\(class\\|def\\|function\\|if\\|elif\\|else\\|for\\|while\\|try\\|switch\\|case\\)\\b.*"
                          prev-code))
                (setq indent (+ prev-indent offset)))))))))
    (indent-line-to indent)
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
