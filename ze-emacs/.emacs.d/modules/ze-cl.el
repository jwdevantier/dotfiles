(defvar ze-cl/deps '(slime))

(defun ze-cl/init ()
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))

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
		   (smartparens-strict-mode +1)
		   (whitespace-mode -1)))))
  
  (eval-after-load "slime"
    '(progn
       (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	     slime-fuzzy-completion-in-place t
	     slime-enable-evaluate-in-emacs t
	     slime-autodoc-use-multiline-p t
	     slime-auto-start 'always)

       (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
       (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector))))

(provide 'ze-cl)
