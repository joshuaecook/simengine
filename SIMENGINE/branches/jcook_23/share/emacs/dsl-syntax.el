(defvar dsl-mode-syntax-table nil
  "Syntax table used in `dsl-mode' buffers")

(unless dsl-mode-syntax-table
  (let ((tab (make-syntax-table (standard-syntax-table)))) 
    (modify-syntax-entry ?_ "w" tab)
    (modify-syntax-entry ?$ "'" tab)
    ; pattern delimiters
    (modify-syntax-entry ?| "\"" tab)
    ; parentheses
    (modify-syntax-entry ?\( "()" tab)
    (modify-syntax-entry ?\) ")(" tab)
    (modify-syntax-entry ?\[ "(]" tab)
    (modify-syntax-entry ?\] ")[" tab)
    (modify-syntax-entry ?\{ "(}" tab)
    (modify-syntax-entry ?\} "){" tab)
    ; comment delimiters
    (modify-syntax-entry ?/ ". 124b" tab)
    (modify-syntax-entry ?* ". 23" tab)
    (modify-syntax-entry ?\n "> b" tab)

    (setq dsl-mode-syntax-table tab)))


(defvar dsl-keyword-symbols
  '(LF and by class constant constructor do do else elseif end enumeration equation equations exists extends forall foreach foreach function get given global hidden if import in input interface lambdafun let model multifunction namespace of open operator or otherwise output overload parameter public quantity replace returns satisfies set state stateful submodel suchthat then to tunable type var visible when while with))

;; A few keywords are in here because they look like values or
;; functions. Others appear because they are in the standard library.
(defvar dsl-builtin-symbols
  '(assert error false notice print println solver true undefined warning))

(defvar dsl-type-symbols
  '(Binary Boolean Number Object String Tuple Vector forwardeuler rk4
  ode23 ode45))


(defvar dsl-id-pattern
  "\\w+")


;; Cf. Section 35.6.3 Parser State from GNU Emacs Lisp Reference Manual
(defmacro dsl-parser-data (state) `(nth 9 ,state))
(defmacro dsl-parser-bo-comment (state) `(nth 8 ,state))
(defmacro dsl-parser-bo-string (state) `(nth 8 ,state))
(defmacro dsl-parser-comment-style (state) `(nth 7 ,state))
(defmacro dsl-parser-min-paren-depth (state) `(nth 6 ,state))
(defmacro dsl-parser-quote (state) `(nth 5 ,state))
(defmacro dsl-parser-comment (state) `(nth 4 ,state))
(defmacro dsl-parser-string (state) `(nth 3 ,state))
(defmacro dsl-parser-last-complete-sexp (state) `(nth 2 ,state))
(defmacro dsl-parser-bo-paren (state) `(nth 1 ,state))
(defmacro dsl-parser-paren-depth (state) `(nth 0 ,state))

(defun dsl-parser-uninteresting (state)
  (and (= 0 (dsl-parser-paren-depth state))
       (not (or
	     (dsl-parser-last-complete-sexp state)
	     (dsl-parser-string state)
	     (dsl-parser-comment state)))))


(provide 'dsl-syntax)
