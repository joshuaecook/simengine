; muse-matlab.el Evaluates MATLAB code
; Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

(require 'muse-publish)

(defgroup muse-matlab nil
  "Publishing evaluated MATLAB code"
  :group 'muse-matlab)

(defcustom muse-matlab-preamble "addpath('/Users/jcook/Sources/simEngine/trunk/local-install-64');"
  ""
  :type 'string
  :group 'muse-matlab)

(defun muse-matlab-publish-matlab-tag (begin end attr)
  "Invoked by MUSE when encountering a <matlab> tag."
  (let ((text (buffer-substring-no-properties begin end)))
    (when muse-publishing-p
      (delete-region begin end)
      (goto-char (point-min)))
    (let ((evalc (muse-matlab-eval-matlab-code text)))
      (when muse-publishing-p
	(muse-insert-markup 
	 (muse-markup-text 'begin-example)
	 evalc
	 (muse-markup-text 'end-example))))))

(put 'muse-matlab-publish-matlab-tag 'muse-dangerous-tag t)

(defun muse-matlab-eval-matlab-code (code)
  (let ((old-buffer (current-buffer))
	(commands (concat "addpath('/Users/jcook/Sources/simEngine/trunk/doc/muse');"
			  muse-matlab-preamble 
			  "disp(muse_eval(sprintf('"
			  (replace-regexp-in-string
			   "%" "%%"
			   (replace-regexp-in-string 
			    "'" "''"
			    (replace-regexp-in-string 
			     "\n" "\\\\n" 
			     code)))
			  "')));"
			  "quit")))
    (set-buffer (generate-new-buffer "*MATLAB*"))
    (unwind-protect
	(progn
	  (call-process "matlab" nil t nil "-nosplash" "-nodesktop" "-r" commands)
	  (shell-command-on-region (point-min) (point-max) "sed '/<<MUSE</,/>ESUM>>/p;d'" t t)
	  (shell-command-on-region (point-min) (point-max) "sed '1d;$d'" t t)
	  (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer)
      (set-buffer old-buffer))))

;; Hooks into publish
(add-to-list 'muse-publish-markup-tags
	     '("matlab" t t nil muse-matlab-publish-matlab-tag)
	     t)

(provide 'muse-matlab)
;;;; muse-matlab.elݚ