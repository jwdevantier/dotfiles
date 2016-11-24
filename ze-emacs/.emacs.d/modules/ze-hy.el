(defvar ze-hy/deps '(hy-mode eval-in-repl rainbow-delimiters))

(defun ze-hy/init ()
  ;; autoload -only load modules in when hy-mode is requested
  (autoload 'hy-mode "hy-mode" "Major mode for editiing Hy (lisp) code" t)

  ;;ensure .hy files trigger hy-mode
  (add-to-list 'auto-mode-alist '("\\.hy$" . hy-mode))

  (eval-after-load 'hy-mode
    '(progn
       (message "hy-lo")
       (require 'eval-in-repl)
       (require 'eval-in-repl-hy)
       (add-hook 'hy-mode-hook #'rainbow-delimiters-mode)
       (add-hook 'hy-mode-hook
		 (lambda ()
		   (ggtags-mode 1)
		   (add-hook 'after-save-hook #'ggtags-update-hook)))
       (define-key hy-mode-map (kbd "<C-return>") 'eir-eval-in-hy)
       (add-hook
	  'ze-py-venv-change-hook
	  (lambda ()
	    (message "updating hy bin location...")
	    ;; inferior-lisp-program is the var actually used so updating
	    ;; 'hy-mode-inferior-lisp-command' doesn't actually apply once the
	    ;; mode is loaded. We set it only to avoid confusion.
	    (let ((hy-cmd (ze:join (list
				    (expand-file-name "~/hywrap")
				    ze-py-venv-path) " ")))
	      (setq hy-mode-inferior-lisp-command hy-cmd)
	      (setq inferior-lisp-program hy-cmd))))
       (message "hy path: '%s'" hy-mode-inferior-lisp-command))))

(provide 'ze-hy)
