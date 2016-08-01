(defvar ze-elm/deps '(elm-mode))

(defun ze-elm/init ()
  (autoload 'elm-mode "elm-mode" "Major mode for editing elm code" t)

  (add-to-list 'auto-mode-alist '("\\.elm$" . elm-mode))

  (eval-after-load 'elm-mode
    '(progn
       (defun ze-elm-mode-defaults()
         (electric-indent-local-mode nil))

       (setq ze-elm-mode-hook 'ze-elm-mode-defaults)
       (add-hook 'elm-mode-hook
		 (lambda () (run-hooks 'ze-elm-mode-hook))))))

(provide 'ze-elm)
