(defvar ze-php/deps '(php-mode))

(defun ze-php/init ()
  (message "PHP MODE INIT")
  ;; autoload - only load module in when php-mode is requested
  (autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
  ;; ensure .php files trigger php-mode
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

  (eval-after-load 'php-mode
    '(progn
       (message "PHP MODE LOADED")
       (defun ze-php-mode-defaults ()
	 "Default PHP mode settings"
	 (ggtags-mode 1))

       (setq ze-php-mode-hook 'ze-php-mode-defaults)
       (add-hook 'php-mode-hook
		 (lambda ()
		   (run-hooks 'ze-php-mode-hook))))))

(provide 'ze-php)
