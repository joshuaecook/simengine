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
    (muse-matlab-eval-matlab-code text)
    (let ((evalc (buffer-substring-no-properties (point-min) (point-max))))
      (when muse-publishing-p
	(delete-region (point-min) (point-max)))
      (muse-insert-markup 
       (muse-markup-text 'begin-example)
       evalc
       (muse-markup-text 'end-example)))))

(put 'muse-matlab-publish-matlab-tag 'muse-dangerous-tag t)

; FIXME need to escape quotes within CODE
(defun muse-matlab-eval-matlab-code (code)
  (let ((commands (concat muse-matlab-preamble 
			  "result = evalc(sprintf('" 
			  (replace-regexp-in-string "\n" "\\\\n" code)
			  "'));"
			  "disp(['<<EVALC<' result '>CLAVE>>']);"
			  "quit")))
    (call-process "matlab" nil t nil "-nosplash" "-nodesktop" "-r"
		  commands)
    (shell-command-on-region (point-min) (point-max) "sed '/<<EVALC</,/>CLAVE>>/p;d'" t t)
    (shell-command-on-region (point-min) (point-max) "sed '1d;$d'" t t)))

;; Hooks into publish
(add-to-list 'muse-publish-markup-tags
	     '("matlab" t t nil muse-matlab-publish-matlab-tag)
	     t)

(provide 'muse-matlab)
;;;; muse-matlab.elݚ