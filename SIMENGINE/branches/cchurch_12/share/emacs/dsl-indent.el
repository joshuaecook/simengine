; Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C

(require 'dsl-syntax)
;; Abbreviations and common variable names
;; paboi: point at beginning of indentation
;; state: an Emacs parser state object. Cf `dsl-syntax.el'
;;
;; Nb any function beginning with `dsl-indent' may move the point.

(defcustom dsl-default-indent 2
  "Number of columns for each level of indentation."
  :group 'dsl
  :type '(integer))


(defun dsl-indentation-for-parser-state (paboi state)
  "Returns the appropriate indentation column for the line containing
  paboi with a given parser state."
  (cond
   ;; A few special cases can be matched base on parser state data alone.
   ((dsl-parser-string state)
    (dsl-indentation-for-string paboi state))
   ((dsl-parser-comment state)
    (dsl-indentation-for-comment paboi state))
   ((dsl-parser-bo-paren state)
    (dsl-indentation-for-paren paboi state))
   ;; For the rest, more complex matching is needed.
   (t 
    (dsl-indentation-for-default paboi state))))

(defun dsl-indentation-for-string (paboi state)
  "Returns the indentation column for a line within a multiline string."
  ;; Strings continued across multiple lines are left-justified.
  0)

(defun dsl-indentation-for-comment (paboi state)
  "Returns the indentation column for a line within a multiline
comment."
  (goto-char paboi)
  (if (not (looking-at "*")) (dsl-indent-relative paboi)
    ;; Multiline comments are aligned with the `*' in `/*'
    (goto-char (dsl-parser-bo-comment state))
    (+ 1 (current-column))))

(defun dsl-indentation-for-paren (paboi state)
  "Returns the proper indentation level for a line within a
parenthetical."
  ;; Align one column right of the open parenthesis.
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



(defconst dsl-indent-begin-block-keywords-regexp
  "\\_<\\(namespace\\|class\\|model\\|if\\|else\\|elseif\\|get\\|set\\|property\\|constructor\\|equations\\)\\_>")

(defconst dsl-indent-end-block-keywords-regexp
  "\\_<end\\|else\\|elseif\\_>")

(defun dsl-indentation-for-default (paboi state)
  (goto-char paboi)
  (cond
   ;; At the end of a block, I should be able to search backwards for
   ;; the keyword that began the block and use that indentation.
   ((and (looking-at dsl-indent-end-block-keywords-regexp) 
	 (dsl-parser-last-complete-sexp state))
    (goto-char (dsl-parser-last-complete-sexp state))
    (back-to-indentation)
    (if (looking-at dsl-indent-begin-block-keywords-regexp)
	(current-indentation)
      ;; Stumped. Try to unindent by one step or bump against the left margin.
      (max 0 (- (current-indentation) dsl-default-indent))))

   ;; If not at the end of a block, the point is probably within a block.
   ((dsl-parser-last-complete-sexp state)
    (goto-char (dsl-parser-last-complete-sexp state))
    (back-to-indentation)
    (let ((eol (save-excursion (end-of-line) (forward-comment -1) (point))))
      (cond
       ;; If the previous parsed expression is on a line that
       ;; begins a block, increment the indentation relative to the
       ;; previous line.
       ((looking-at dsl-indent-begin-block-keywords-regexp)
	(+ dsl-default-indent (current-indentation)))
       
       ((and (looking-at "\\_<\\(overload\\|function\\|get\\|set\\|constant\\|var\\)\\_>")
	     (dsl-equals-at-eol))
	(message "match-end %s" (- (match-end 0) (current-indentation)))
	(+ 1 (current-indentation) (- (match-end 0) (match-beginning 0))))
       
       ;; If the previous line was not functional definition,
       ;; increment the indentation relative to the previous line.
       ((and (looking-at "\\_<\\(overload\\|function\\|get\\|set\\)\\_>")
	     (not (search-forward "=" eol t)))
	(+ dsl-default-indent (current-indentation)))

       ((and (looking-at "\\_<\\(foreach\\)\\_>")
	     (re-search-forward "\\_<do\\_>" eol t))
	(+ dsl-default-indent (current-indentation)))
       
       ;; FIXME how to handle multifunction?

       (t (current-indentation)))))

   ;; If all else fails, indent to the same level as the previous expression
   (t (current-indentation))))


(defun dsl-point-at-boi (&optional line)
  "Returns the point at the beginning of indentation at LINE."
  (save-excursion
    (when line (goto-line line))
    (back-to-indentation)
    (point)))

(defun dsl-calculate-indentation ()
  "Returns the proper indentation column for the current line."
  (let* ((inhibit-point-motion-hooks t)
	 (paboi (dsl-point-at-boi)))
    (if (= 0 paboi) 0 ; Beginning of buffer. First line is always left-justified.
      (save-excursion ; Start at the current indentation and try to figure it out.
	(dsl-indentation-for-parser-state 
	 paboi (parse-partial-sexp (point-min) paboi))))))


(defun dsl-indent-line ()
  "Indents the current line."
  (interactive)
  (let ((column (dsl-calculate-indentation)))
    (if (<= (current-column) (current-indentation))
	(indent-line-to column)
      (save-excursion (indent-line-to column)))))


(provide 'dsl-indent)