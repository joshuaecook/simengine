;; Emacs mode for editing DSL files
;;
;; Installation
;; ============
;;
;; Modify your Emacs initialization file:
;; 1. Add the path to this file to `load-path'.
;; 2. Instruct Emacs to use `dsl-mode' for editing DSL files:
;;    (autoload 'dsl-mode "dsl" "Major mode for editing DSL files" t)
;;    (add-to-list 'auto-mode-alist '("\\.dsl\\'" . dsl-mode))
;; 3. Adjust your preferences in the `dsl' group of `customize-group'.
;;