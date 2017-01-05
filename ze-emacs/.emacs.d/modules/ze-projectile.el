(defvar ze-projectile/deps '(projectile helm-ag helm-projectile))

(defun ze-projectile/init ()
  (require 'projectile)
  (setq projectile-cache-file (expand-file-name  ".projectile.cache" "~"))
  (projectile-global-mode +1)

  (->>
   '(("H-n g" . helm-do-grep-ag)
     ("H-n f" . helm-projectile-find-file)
     ("H-n c" . helm-M-x))
   (ze:bind projectile-mode-map)))

(provide 'ze-projectile)
