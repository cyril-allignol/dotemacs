;; (use-package smie)
(require 'smie)

(defvar imp-mode-hook nil)

(defvar imp-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Imp major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.imp\\'" . imp-mode))

(defvar imp-font-lock
  (let* (
         (imp-keywords '("if" "then" "else" "fi" "while" "do" "end" "new" "in"
                           ))
         (imp-constants '("True" "False"))
         (imp-builtins '("skip"))

         (imp-keywords-regexp (regexp-opt imp-keywords 'words))
         (imp-constants-regexp (regexp-opt imp-constants 'words))
         (imp-builtins-regexp (regexp-opt imp-builtins 'words))
         )
    `((,imp-constants-regexp . font-lock-constant-face)
      (,imp-builtins-regexp . font-lock-builtin-face)
      (,imp-keywords-regexp . font-lock-keyword-face)
      (";;\\|:=" . font-lock-keyword-face)
      ("(\\|)" . font-lock-doc-face)
      ;; note: order above matters, because once colored, that part won't change.
      ;; in general, put longer words first
      ))
  "Default highlighting expressions for Imp mode")

(defvar imp-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for imp-mode")

(defvar imp-tab-width 2)

(defconst imp-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (instr ("skip")
             (expr)
             ("new" id ":=" expr "in" instr)
             (id ":=" expr)
             (instr ";;" instr)
             ("if" expr "then" instr "else" instr "fi")
             ("while" expr "do" instr "end"))
      (expr (id)
            (expr "+" expr) (expr "-" expr)
            (expr "*" expr) (expr "/" expr) (expr "%" expr)
            (expr "=" expr) (expr "!=" expr)
            (expr "<" expr) (expr "<=" expr) (expr ">" expr) (expr ">=" expr)
            (expr "|" expr) (expr "&" expr) ("~" expr)))
    '((assoc "in")
      (assoc ";;")
      (assoc "|")
      (assoc "&")
      (assoc "=" "!=" "<" "<=" ">" ">=")
      (assoc "+" "-")
      (assoc "*" "/" "%")
      (nonassoc "~")))))

(defun imp-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) imp-tab-width)
    (`(,_ . ";;") (smie-rule-separator kind))
    (`(:after . ":=") imp-tab-width)
    (`(:before . ,(or `"do" `"("))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else") (smie-rule-parent)))))

(define-derived-mode imp-mode fundamental-mode "Imp"
  "Major mode for editing Imp files."
  :syntax-table imp-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(imp-font-lock))
  (smie-setup imp-smie-grammar 'imp-smie-rules)
  (setq-local comment-start "#")
  (setq-local comment-end "\n")
  )

(provide 'imp-mode)
