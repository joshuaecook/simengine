(require 'dsl-syntax)
;; Abbreviations and common variable names
;; paboi: point at beginning of indentation
;; state: an Emacs parser state object. Cf `dsl-syntax.el'


(defcustom dsl-default-indent 4
  "Number of columns for each level of indentation."
  :group 'dsl
  :type '(integer))


(defun dsl-indentation-for-parser-state (paboi state)
  (cond
   ((dsl-parser-string state)
    (dsl-indentation-for-string paboi state))
   ((dsl-parser-comment state)
    (dsl-indentation-for-comment paboi state))
   ((dsl-parser-bo-paren state)
    (dsl-indentation-for-paren paboi state))

   (t 
    (dsl-default-indentation paboi state))))

(defun dsl-indentation-for-string (paboi state)
  0)

(defun dsl-indentation-for-comment (paboi state)
  "Returns the indentation column for a line within a multiline comment."
  (goto-char paboi)
  (if (not (looking-at "*")) (dsl-indent-relative paboi)
    (goto-char (dsl-parser-bo-comment state))
    (+ 1 (current-column))))

(defun dsl-indentation-for-paren (paboi state)
  "Returns the proper indentation level for a line within a parenthetical."
  (goto-char (dsl-parser-bo-paren state))
  (+ 1 (current-column)))

(defun dsl-indent-relative (&optional point)
  (when point (goto-char point))
  (let ((indent-line-function 'indent-relative))
    (indent-according-to-mode)
    (current-indentation)))

(defun dsl-equals-at-eol (&optional line)
  (save-excursion
    (when line (goto-line line))
    (end-of-line)
    (forward-comment -1)
    (eq ?= (char-before))))

(defun dsl-default-indentation (paboi state)
  (goto-char paboi)
  (cond
   ((and (looking-at "\\_<end\\|else\\|elseif\\_>") (dsl-parser-last-complete-sexp state))
    (goto-char (dsl-parser-last-complete-sexp state))
    (back-to-indentation)
    (if (looking-at "\\_<\\(namespace\\|class\\|model\\|if\\|else\\|get\\|set\\|property\\|constructor\\|equations\\)\\_>")
	(current-indentation)
      (max (- (current-indentation) dsl-default-indent) 0)))

   ((dsl-parser-last-complete-sexp state)
    (goto-char (dsl-parser-last-complete-sexp state))
    (back-to-indentation)
    (let ((eol (save-excursion (end-of-line) (forward-comment -1))))
      (cond
       ((looking-at "\\_<\\(namespace\\|class\\|model\\|property\\|if\\|else\\|elseif\\|get\\|set\\|constructor\\|equations\\)\\_>")
	(+ dsl-default-indent (current-indentation)))

       ((and (looking-at "\\_<\\(overload\\|function\\|constant\\|var\\)\\_>")
	     (eq ?= (char-before eol)))
	(+ dsl-default-indent (current-indentation)))
     
       ((and (looking-at "\\_<\\(overload\\|function\\)\\_>")
	     (not (search-forward "=" eol t)))
       (+ dsl-default-indent (current-indentation)))

       (t (current-indentation)))))

    (t (current-indentation))))


(defun dsl-indent-find-tab-stop-rec (column stop-list)
  (cond
   ((not stop-list) column)
   ((< column (car stop-list)) (car stop-list))
   ((not (cdr stop-list)) column)
   ((< column (cadr stop-list)) (cadr stop-list))
   (t
    (dsl-indent-find-tab-stop-rec column (cdr stop-list)))))

(defun dsl-indent-find-tab-stop (column)
  (dsl-indent-find-tab-stop-rec column tab-stop-list))

(defun dsl-point-at-boi (&optional line)
  "Returns the point at the beginning of indentation at LINE."
  (save-excursion
    (when line (goto-line line))
    (back-to-indentation)
    (point)))

(defun dsl-calculate-indentation ()
  "Returns the proper indentation level for the current line."
  (let* ((inhibit-point-motion-hooks t)
	 (paboi (dsl-point-at-boi)))
    (save-excursion
      (if (= 0 paboi) 0 ; beginning of buffer. First line is always 
        ; or else start at the current indentation and try to figure it out.
	(dsl-indentation-for-parser-state 
	 paboi (parse-partial-sexp (point-min) paboi))))))


(defun dsl-indent-line ()
  "Indents the current line."
  (interactive)
  (let ((column (save-excursion (dsl-calculate-indentation))))
    (if (<= (current-column) (current-indentation))
	(indent-line-to column)
      (save-excursion (indent-line-to column)))))


(provide 'dsl-indent)