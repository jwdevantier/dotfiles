(defvar ze-ts/deps '(tide web-mode flycheck js2-mode))

(defun ze-ts/setup-mode ()
  (interactive)
  (message "tide mode initiated")
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(defun ze-ts/init ()
  (add-hook 'typescript-mode-hook
	    #'ze-ts/setup-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq tide-format-options
	'(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
	  :placeOpenBraceOnNewLineForFunctions nil))

  ;; TSX support
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(ze-ts/setup-mode))))

  ;;JavaScript
  (add-hook 'js2-mode-hook #'ze-ts/setup-mode)

  ;;JSX
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "jsx" (file-name-extension buffer-file-name))
		(ze-ts/setup-mode))))

  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (eval-after-load 'tide-mode
    '(progn (ze-ts/setup-mode)
	    (ze:bind tide-mode-map
		     '(("H-r r" . tide-restart-server)
		       ("H-r d" . tide-documentation-at-point)
		       ("H-r u" . tide-references)
		       ("H-r n" . tide-rename-symbol)
		       ("H-r f" . tide-format)
		       ("H-r k" . tide-compile-file)
		       ("H-r K" . tide-auto-compile-file)))
	    (ze:bind tide-references-mode-map
		     '(("H-down" . tide-find-next-error)
		       ("H-up" . tide-find-previous-error)))
	    (message "tide mode triggered"))))

(provide 'ze-ts)
