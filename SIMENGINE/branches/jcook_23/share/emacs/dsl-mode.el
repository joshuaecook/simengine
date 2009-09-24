(require 'custom)
(require 'dsl-syntax)
(require 'dsl-indent)

(defvar dsl-mode-hook nil)

(defgroup dsl nil
  "Custom options for DSL mode"
  :group 'languages
  :prefix "dsl-")



(defvar dsl-mode-map nil
  "Keymap used in `dsl-mode' buffers")
(unless dsl-mode-map
  (let ((map (make-sparse-keymap)))

    ;; TODO make these compatible with simEngine
    ;; (define-key map "\C-c\C-e" 'dynamo-send-string)
    ;; (define-key map "\C-c\C-v" 'dynamo-send-region)
    ;; (define-key map "\C-c\C-c" 'dynamo-send-buffer)
    ;; (define-key map "\C-c\C-i" 'dynamo-import-file)
    ;; (define-key map "\C-c!" 'run-dynamo)

    (setq dsl-mode-keymap map)))


(defun dsl-mode-make-optimized-symbol-pattern (syms)
  "Constructs an optimal regexp to match any of a list of symbols."
  (concat "\\_<"
	  (regexp-opt (mapcar 'symbol-name syms) t)
	  "\\_>"))

(defun dsl-enumerate (items &optional start)
  (unless start (setq start 0))
  (if items (cons (list start (car items))
		  (dsl-enumerate (cdr items) (+ 1 start)))
    nil))

(defun dsl-definition-spec-to-font-lock-keywords (spec)
   (if (or (symbolp spec) (not (cdr spec)))
       `(,(concat "\\_<" (symbol-name spec) "\\_>") 
	 (0 font-lock-keyword-face))
     (let ((ids (dsl-enumerate (cdr spec) 2)))
       `(,(concat "\\(\\_<" (symbol-name (car spec)) "\\_>\\)\\s-+"
		  (mapconcat 
		   (lambda (_) (concat "\\(" dsl-id-pattern "\\)"))
		   ids "\s+")) 
	 (1 font-lock-keyword-face)
	 ,@ids))))

(defvar dsl-mode-font-lock-keywords nil)
(eval-when-compile
  (unless dsl-mode-font-lock-keywords
    (setq dsl-mode-font-lock-keywords
	  `(
;; 	    ,@(mapcar 'dsl-definition-spec-to-font-lock-keywords
;;  		      '(
;;  			(function font-lock-function-name-face)
;;  			(namespace dsl-namespace-name-face)
;;  			(class font-lock-type-face)
;;  			(constant font-lock-constant-face)
;;  			(var font-lock-variable-face)
;;  			(state font-lock-variable-face)
;;  			(input font-lock-variable-face)
;;  			(output font-lock-variable-face)
;;  			(submodel font-lock-type-face font-lock-variable-face)
;;  			))

	    ("\\(model\\)\\s-+(\\w+\\(?:\\s-*,\\s-*\\w+\\)*)\\s-*=\\s-*\\(\\w+\\)"
	     (1 font-lock-keyword-face)
	     (2 font-lock-type-face))

;;	    ("\\<\\(state\\)\\>\\s-*\\(\\w+\\)"
;;	     (1 font-lock-keyword-face)
;;	     (2 font-lock-variable-face))

	    ("\\(\\-?\\<[0-9.]+\\>\\)"
	     (1 font-lock-constant-face))

	    (,(dsl-mode-make-optimized-symbol-pattern dsl-keyword-symbols) 
	     (0 font-lock-keyword-face))

	    (,(dsl-mode-make-optimized-symbol-pattern dsl-builtin-symbols) 
	     (0 font-lock-builtin-face))

	    (,(dsl-mode-make-optimized-symbol-pattern dsl-type-symbols) 
	     (0 font-lock-type-face))
	    ))

))




(defvar dsl-mode-imenu-generic-expression nil)
(unless dsl-mode-imenu-generic-expression
  (setq dsl-mode-imenu-generic-expression
	'(;;("*Functions*" "\\(function\\)\\s-+\\(\\w+\\)" 2)
	  ("Namespaces" "\\(namespace\\)\\s-+\\(\\w+\\)" 2)
	  ("Classes" "\\(class\\)\\s-+\\(\\w+\\)" 2)
	  ("Models" "\\(model\\)\\s-+(\\w+\\(?:\\s-*,\\s-*\\w+\\)*)\\s-*=\\s-*\\(\\w+\\)" 2))))




(define-derived-mode dsl-mode fundamental-mode "DSL"
  "Major mode for editing DSL source code"
  (set (make-local-variable 'font-lock-defaults) 
       '((dsl-mode-font-lock-keywords)
	 nil nil
	 ((?' . "w"))
	 nil))

  (set (make-local-variable 'imenu-generic-expression) 
       dsl-mode-imenu-generic-expression)

  (set (make-local-variable 'indent-line-function) 'dsl-indent-line)
  (set (make-local-variable 'require-final-newline) t)

  (set (make-local-variable 'comment-start) "//")

  (use-local-map dsl-mode-map)
  (imenu-add-to-menubar "DSL")
  )

(add-to-list 'auto-mode-alist '("\\.dsl\\'" . dsl-mode))

(provide 'dsl-mode)