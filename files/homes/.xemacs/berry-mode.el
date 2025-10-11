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
  "Indent current line for Berry Script."
  (interactive)
  (let ((indent 0)
        (not-indented t)
        (offset 2))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; Dedent for block-ending keywords
       ((looking-at "\\s-*\\(end\\|elif\\|else\\|case\\|default\\|catch\\)\\b")
        (save-excursion
          (forward-line -1)
          (setq indent (current-indentation))
          (setq indent (max 0 (- indent offset)))))
       ;; Align within parentheses
       ((nth 1 (syntax-ppss))
        (goto-char (nth 1 (syntax-ppss)))
        (forward-char 1)
        (skip-syntax-forward "- ")
        (setq indent (current-column)))
       ;; Increase indent after block openers
       (t
        (save-excursion
          (forward-line -1)
          (setq indent (current-indentation))
          (when (looking-at ".*\\<\\(class\\|def\\|function\\|if\\|elif\\|else\\|for\\|while\\|try\\|switch\\|case\\)\\b.*")
            (setq indent (+ indent offset)))))))
    (indent-line-to indent)
    (when (< (point) (+ (line-beginning-position) indent))
      (goto-char (+ (line-beginning-position) indent)))))

;;;###autoload
(define-derived-mode berry-mode prog-mode "Berry"
  "Major mode for editing Berry script files."
  :syntax-table berry-mode-syntax-table
  (setq-local font-lock-defaults '(berry-font-lock-keywords))
  (setq-local indent-line-function 'berry-indent-line))

(provide 'berry-mode)
;;; berry-mode.el ends here
