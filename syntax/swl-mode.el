;;; swl-mode.el --- Major Mode for swl source code -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;Major Mode for swl source code

;;; Code:
(defconst swl-mode-syntax-table
  (let ((st (make-syntax-table)))
;    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    (modify-syntax-entry ?_ "w" st)

    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)

    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\n "> b"  st)

    st))

(eval-and-compile
    (defconst swl-keywords
        '("if" "else" "func" "alias" "intrinsic" "as" "arr" "while" "struct" "ref" "deref" "include" "break" "<-" "from" "for" "typeclass" "instance")
        ))

(eval-and-compile
    (defconst swl-types
        '("char" "bool" "int" "float" "void" "true" "false")
        ))

(defconst swl-font-lock-keywords
  `((,(regexp-opt swl-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt swl-types 'symbols) . font-lock-type-face)
    ("\\([a-zA-Z_]+\\**\\)[^a-zA-Z_, :\t\n()]* +[a-zA-Z_]+" 1 font-lock-type-face)
    ("alias[ \t\n]+\\([^ \t\n]*\\)[ \t\n]*=[ \t\n]*.*" 1 font-lock-type-face)
    ("alias[ \t\n]+[^ \t\n]*[ \t\n]*=[ \t\n]*\\[*\\([^ \t\n]*?\\)\\]*;" 1 font-lock-type-face)
    ("func \\(.*?\\)[( \t\n]" 1 font-lock-function-name-face)
    ("->[ \t\n]*\\([^ \t\n]*\\)[ \t\n]*" 1 font-lock-type-face)
    ("\\([a-zA-Z_]*\\)[ \t\n]*{" 1 font-lock-type-face)
    ("[ \t\n]+struct \\(.*?\\)[( \t\n)]" 1 font-lock-type-face))
)

;;;###autoload
(define-derived-mode swl-mode prog-mode "swl"
  "Major mode for swl source files."
  :syntax-table swl-mode-syntax-table
  (setq font-lock-defaults '(swl-font-lock-keywords))
  (setq-local indent-line-function 'insert-tab)
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.swl\\'" . swl-mode))

(provide 'swl-mode)

;;; swl-mode.el ends here
