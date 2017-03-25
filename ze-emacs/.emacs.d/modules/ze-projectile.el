(defvar ze-projectile/deps '(projectile helm-ag helm-projectile))

(defun ze-projectile/init ()
  (require 'projectile)
  (setq projectile-cache-file (expand-file-name  ".projectile.cache" "~"))
  (projectile-global-mode +1)

  (add-hook 'projectile-mode-hook
	    (lambda ()
	      (helm-projectile-on)))
  (add-hook 'helm--minor-mode-hook
	    'helm-projectile-on)

  (->>
   '(("H-n g" . helm-do-grep-ag)
     ("H-n f" . helm-projectile-find-file)
     ("H-n c" . helm-M-x)
     ("C-x b" . helm-mini))
   (ze:bind projectile-mode-map)))

(provide 'ze-projectile)
