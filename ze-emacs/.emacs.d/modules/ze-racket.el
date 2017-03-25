(defvar ze-racket/deps '(racket-mode))

(defvar ze-racket/racket-home "~/apps/racket")

(defun ze-racket/init ()
  (autoload 'racket-mode "racket-mode" "Major mode for editing Racket code" t)
  (add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))

  (eval-after-load 'racket-mode
    '(progn
       (defun ze-racket-mode-defaults ()
	 "Default racket settings"
	 ;; find the racket & raco bindings
	 (let ((racket-home (expand-file-name ze-racket/racket-home)))
	   (let ((racket-bin (mapconcat 'identity `(,racket-home "bin" "racket") "/")))
	     (setq racket-racket-program racket-bin))
	   (let ((raco-bin (mapconcat 'identity `(,racket-home "bin" "raco") "/")))
	     (setq racket-raco-program raco-bin)))
	 ;; bind keys
	 ;; See https://github.com/greghendershott/racket-mode/blob/master/Reference.md
	 (ze:bind racket-mode-map
		  '(;; REPL interaction
		    ;; ----------------
		    ;; Start REPL, or switch to it
		    ("H-r r" . racket-repl)
		    ;; Run module code with detailed error stacktraces
		    ("H-r R" . racket-run-with-errortrace)
		    ;; loads into the ctx of the current module (@ point)
		    ;; and runs its code.
		    ("H-r p" . racket-run-and-switch-to-repl)
		    ("H-r D" . racket-run-with-errortrace)
		    ("H-r k" . racket-run)
		    ("H-r e" . racket-send-last-sexp)
		    ;;eval top-level expression enclosing point
		    ("H-r d" . racket-send-definition)
		    ;; runs program in profile mode (g to refresh stats)
		    ;; (repl fn's will be profiled, too)
		    ("H-r p" . racket-profile)

		    ;; Help / Documentation
		    ;; --------------------
		    ;;in-buffer help
		    ("H-h s" . racket-describe)
		    ("H-h f" . racket-describe)
		    ;; external docs lookup
		    ("H-h h" . racket-doc)

		    ;; buffer-related actions
		    ;; ----------------------
		    ;; run unit-tests from nested test module
		    ("H-b t" . racket-test)
		    ;; auto-completion on requiring modules
		    ("H-b r" . racket-open-require-path)
		    ;; #lang racket => #lang racket/base + requirements
		    ;; (recommended performance/footprint optimization)
		    ("H-b R" . racket-base-requires)
		    ;; check syntax of buffer
		    ("H-b s" . racket-check-syntax-mode)

		    ;; Cross-reference / Navigational
		    ;; ------------------------------
		    ("H-x m" . racket-visit-module)

		    ;; Macro expansion
		    ;; ---------------
		    ("H-m e" . racket-expand-last-sexp)
		    ("H-m E" . racket-expand-again)
		    ("H-m r" . racket-expand-region)
		    ("H-m d" . racket-expand-definition))))

       ;; can be overridden if need be
       (setq ze-racket-mode-hook 'ze-racket-mode-defaults)
       (add-hook 'racket-mode-hook
		 (lambda () (run-hooks 'ze-racket-mode-hook))))))

(provide 'ze-racket)
