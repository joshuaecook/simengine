
;; Function to launch a process under GDB (or specified debugger).
;;
;; Launches a process under debugger, sets breakpoint on 'main' and
;; runs with the specified arguments.

(defun debugit (path exec args &optional debugger)
  (unless debugger (setq debugger "gdb"))
  (gdb (concat debugger " --annotate=3 " exec))
  (gud-basic-call (concat "cd " path))  
  (gud-basic-call "b main")
  (gud-basic-call (concat "r " args))
  )

;; Function to launch Matlab in the specified path
;;
;; To be used with the bash script 'edb'

(defun matdebug (exec path)
  (gdb (concat "gdb --annotate=3 " exec))
  (gud-basic-call (concat "cd " path))
  (gud-basic-call "r -nodisplay")
  )