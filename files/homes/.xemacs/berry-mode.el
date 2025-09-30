;;; berry-mode.el --- Major mode for editing Berry script files -*- lexical-binding: t; -*-
;;
;; Minimal Emacs major mode for the Berry scripting language
;; Provides: syntax highlighting (font-lock), basic indentation, and file association
;; Installation: drop this file in your load-path and add
;;   (require 'berry-mode)
;;   (add-to-list 'auto-mode-alist '("\\.be\\'" . berry-mode))
;;
;;; Code:

(require 'prog-mode)

(defgroup berry-mode nil
  "Major mode for editing Berry scripts."
  :prefix "berry-"
  :group 'languages)

;; Keywords collected from the Berry reference manual and examples.
(defvar berry-keywords
  '("if" "elif" "else" "end" "for" "while" "do" "break" "continue"
    "return" "function" "def" "class" "var" "import" "from" "try" "catch"
    "throw" "switch" "case" "default" "in" "and" "or" "not"))

(defvar berry-builtins
  '("print" "input" "assert" "type" "size" "int" "real" "bool" "str"
    "list" "map" "range" "super" "classname" "classof"))

(defvar berry-constants
  '("true" "false" "nil"))

(defun berry--regexp-from-keywords (keywords)
  "Return regexp matching any of KEYWORDS as a symbol."
  (concat "\\b" (regexp-opt keywords t) "\\b"))

(defvar berry-font-lock-keywords
  `((,(berry--regexp-from-keywords berry-keywords) . font-lock-keyword-face)
    (,(berry--regexp-from-keywords berry-builtins) . font-lock-builtin-face)
    (,(berry--regexp-from-keywords berry-constants) . font-lock-constant-face)
    ;; double-quoted strings
    ("\"\\(\\\\.\\|[^\"\\]\\)*\"" . font-lock-string-face)
    ;; single quoted words (simplistic)
    ("'\\w+" . font-lock-variable-name-face)
    ;; function definitions
    ("\\<\\(def\\|function\\)\\s-+\\([A-Za-z_]\\w*\)" 2 font-lock-function-name-face)))

;; Comment syntax: Berry examples use `#` for line comments.
(defvar berry-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?' "'" st)
    st)
  "Syntax table for `berry-mode'.")

(defvar berry-indent-offset 4
  "Indentation offset for `berry-mode'.")

(defun berry--line-starts-block-p (line)
  "Return non-nil if LINE starts a block that should increase indent."
  (string-match-p "^\\s-*\\(if\\|for\\|while\\|function\\|def\\|class\\|do\\|try\\|switch\\)\\b" line))

(defun berry--line-ends-block-p (line)
  "Return non-nil if LINE closes a block (contains `end')."
  (string-match-p "^\\s-*end\\b" line))

(defun berry-calc-indentation ()
  "Compute indentation for current line in `berry-mode'."
  (let ((nest 0))
    (save-excursion
      (while (not (bobp))
        (forward-line -1)
        (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
          (unless (or (string-empty-p line) (string-prefix-p "#" line))
            (when (berry--line-starts-block-p line)
              (setq nest (1+ nest)))
            (when (berry--line-ends-block-p line)
              (setq nest (1- nest)))))))
    ;; Adjust for current line
    (let ((curr (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
      (when (berry--line-ends-block-p curr)
        (setq nest (1- nest))))
    (max 0 (* berry-indent-offset nest))))

(defun berry-indent-line ()
  "Indent current line for `berry-mode'."
  (interactive)
  (let ((indent (berry-calc-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent))
    (when (< (current-column) (current-indentation))
      (back-to-indentation))))

;;;###autoload
(define-derived-mode berry-mode prog-mode "Berry"
  "Major mode for editing Berry script files."
  :syntax-table berry-mode-syntax-table
  (setq-local font-lock-defaults '(berry-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-line-function 'berry-indent-line)
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode 1)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.be\\'" . berry-mode))

(provide 'berry-mode)
;;; berry-mode.el ends here
