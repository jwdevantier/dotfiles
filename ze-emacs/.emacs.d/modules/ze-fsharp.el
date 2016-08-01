
(defvar ze-fsharp/deps '(fsharp-mode))

(defun ze-fsharp/init ()
  ;; autoload - only load module in when fsharp-mode is requested
  (autoload 'fsharp-mode "fsharp-mode" "Major mode for editing F# code." t)
  ;; ensure .fs/.fsx/.fsi etc files trigger fsharp-mode
  (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))

  ;; Delay configuration until after fsharp-mode is loaded
  (eval-after-load 'fsharp-mode
    '(progn
       (defun ze-fsharp-mode-defaults ()
	 "Default fsharp-mode settings - override ze-fsharp-mode-hook to change"
	 ;; Default locations on Ubuntu Linux
	 (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
	 (setq fsharp-compiler "/usr/bin/fsharpc")

	 ;; 4=>2 spaces of indentation - much more room for code.
	 (setq fsharp-indent-offset 2)
	 (setq fsharp-continuation-offset 2))
       
       (setq ze-fsharp-mode-hook 'ze-fsharp-mode-defaults)
       (add-hook 'fsharp-mode-hook
		 (lambda ()
		   (run-hooks 'ze-fsharp-mode-hook))))))

(provide 'ze-fsharp)
