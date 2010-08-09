; muse-subsup.el Adds superscript and subscript to MUSE documents
; Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

(require 'muse-publish)

(defgroup muse-subsup nil
  "Superscript and subscript for MUSE documents.")

(defun muse-subsup-sup-tag (begin end &optional attr)
  "Invoked by MUSE when encountering a <sup> tag."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-max))
    (when muse-publishing-p
      (muse-insert-markup
       (cond
	((or (muse-style-derived-p "latex") (muse-style-derived-p "context"))
	 "}}$")
	((or (muse-style-derived-p "html") (muse-style-derived-p "xml"))
	 "</sup>")))
      (goto-char (point-min))
      (muse-insert-markup
       (cond
	((or (muse-style-derived-p "latex") (muse-style-derived-p "context"))
	 "$^{\\textrm{")
	((or (muse-style-derived-p "html") (muse-style-derived-p "xml"))
	 "<sup>")))
      (goto-char (point-max)))))

(defun muse-subsup-sub-tag (begin end &optional attr)
  "Invoked by MUSE when encountering a <sub> tag."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-max))
    (when muse-publishing-p
      (muse-insert-markup
       (cond
	((or (muse-style-derived-p "latex") (muse-style-derived-p "context"))
	 "}}$")
	((or (muse-style-derived-p "html") (muse-style-derived-p "xml"))
	 "</sub>")))
      (goto-char (point-min))
      (muse-insert-markup
       (cond
	((or (muse-style-derived-p "latex") (muse-style-derived-p "context"))
	 "$_{\\textrm{")
	((or (muse-style-derived-p "html") (muse-style-derived-p "xml"))
	 "<sub>")))
      (goto-char (point-max)))))

;; Hooks into publish
(add-to-list 'muse-publish-markup-tags
	     '("sub" t nil t muse-subsup-sub-tag)
	     t)

(add-to-list 'muse-publish-markup-tags
	     '("sup" t nil t muse-subsup-sup-tag)
	     t)


(provide 'muse-subsup)
;;;; muse-subsup.el
