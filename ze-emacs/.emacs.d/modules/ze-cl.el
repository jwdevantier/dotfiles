(defvar ze-cl/deps '(slime))

(defun ze-cl/init ()
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))

  (add-hook
   'lisp-mode-hook
   (lambda ()
     (setq prettify-symbols-alist
	   '(("lambda" . 955) ;; λ
	     ))))
  (global-prettify-symbols-mode 1)

  (eval-after-load 'lisp-mode
    '(progn
       (message "CL (lisp) mode loaded...")

       (setq slime-lisp-implementations
	     '((ccl ("ccl") :coding-system utf-8-unix)
	       (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

       (setq slime-default-lisp
	     (if (and (eq system-type 'darwin)
		      (executable-find "ccl"))
		 'ccl 'sbcl))

       (setq slime-contribs '(slime-fancy))

       (add-hook 'slime-repl-mode-hook
		 (lambda ()
		   (paredit-mode)
		   (whitespace-mode -1)))))
  
  (eval-after-load "slime"
    '(progn
       (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	     slime-fuzzy-completion-in-place t
	     slime-enable-evaluate-in-emacs t
	     slime-autodoc-use-multiline-p t
	     slime-auto-start 'always)

       (dolist (bind '(("TAB" . slime-indent-and-complete-symbol)
		       ("C-c C-s" . slime-selector)
		       ;;Documentation
		       ("H-h s" . slime-describe-symbol)
		       ("H-h f" . slime-describe-function)
		       ("H-h a" . slime-apropos)
		       ("H-h p" . slime-apropos-package)
		       ("H-h d" . slime-hyperspec-lookup)
		       ;;X-ref
		       ("H-x c" . slime-who-calls)
		       ("H-x w" . slime-calls-who)
		       ("H-x r" . slime-who-references)
		       ("H-x b" . slime-who-binds)
		       ("H-x s" . slime-who-sets)
		       ("H-x m" . slime-who-macroexpands)
		       ;; Profiling
		       ("H-p f" . slime-toggle-profile-fdefinition)
		       ("H-p p" . slime-profile-package)
		       ("H-p s" . slime-profile-by-substring)
		       ("H-p q" . slime-unprofile-all)
		       ("H-p r" . slime-profile-report)
		       ("H-p c" . slime-profile-reset)
		       ;; REPL - amalgation of categories
		       ("H-r r" . slime)
		       ("H-r k" . slime-compile-and-load-file)
		       ("H-r e" . slime-eval-last-expression) ;; C-c C-e ?
		       ("H-r s" . slime-sync-package-and-default-directory)

		       ("H-r d" . slime-eval-defun)
		       ("H-r t" . slime-toggle-trace-fdefinition)
		       ("H-r T" . slime-untrace-all)

		       ("H-r p" . slime-repl-set-package)
		       ("H-r P" . slime-cd)

		       ("H-r m" . slime-macroexpand-1)
		       ("H-r M" . slime-macroexpand-all)))
	 (define-key slime-mode-map (kbd (car bind)) (cdr bind))))))

(provide 'ze-cl)
