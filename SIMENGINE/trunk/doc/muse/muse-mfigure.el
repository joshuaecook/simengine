; muse-mfigure.el Generates figures from MATLAB code
; Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

(require 'muse-publish)

(defgroup muse-mfigure nil
  "Publishing MATLAB figures"
  :group 'muse-mfigure)

(defcustom muse-mfigure-preamble "addpath('/Users/jcook/Sources/simEngine/trunk/local-install-64');"
  ""
  :type 'string
  :group 'muse-mfigure)


(defun muse-publish-mfigure-tag (begin end attr)
  ""
  (save-restriction
    (narrow-to-region begin end)
    (let* ((text (buffer-substring-no-properties begin end)))
      (when muse-publishing-p
	(delete-region begin end)
	(goto-char (point-min)))
      (let ((figurefile (muse-publish-matlab-publish-code text)))
	(when (and figurefile muse-publishing-p)
	  (muse-insert-markup
	   (muse-markup-text 'comment-begin)
	   text
	   (muse-markup-text 'comment-end)
	   (let ((ext (or (file-name-extension figurefile) ""))
		 (name (muse-path-sans-extension figurefile)))
	     (muse-markup-text 'image name ext)))))
      (goto-char (point-max))))
)

(defun muse-publish-matlab-publish-code (code)
  ""
  (let* ((tmpdir (cond ((boundp 'temporary-file-directory) temporary-file-directory)
		       ((fboundp 'temp-directory) (temp-directory))
		       (t "/tmp")))
	 (mfile (expand-file-name 
		 (concat "muse_mfigure__" (format "%d" (abs (sxhash code))))
		 tmpdir)))
    (with-temp-file (concat mfile ".m")
      (insert code))
    (message "MATLAB publish %s" (concat mfile ".m"))
    (call-process "matlab" nil "*scratch*" nil "-nosplash" "-nodesktop" "-r" 
		  (concat muse-mfigure-preamble "addpath('" tmpdir "'); publish('" mfile ".m', struct('format','html','imageFormat','png','outputDir', '" tmpdir "'))"))
    (if (file-exists-p (concat mfile ".html"))
	(progn
	  (delete-file (concat mfile ".html"))
	  (delete-file (concat mfile ".m"))
	  (concat mfile "_01.png"))
      (error "Failed to publish html file")
      nil)
    )
)


;; Adds the hook into publish
(add-to-list 'muse-publish-markup-tags
	     '("mfigure" t t nil muse-publish-mfigure-tag)
	     t)

(provide 'muse-mfigure)
;;;; muse-mfigure.el 