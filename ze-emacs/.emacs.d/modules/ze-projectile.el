(defvar ze-projectile/deps '(projectile))

(defun ze-projectile/init ()
  (require 'projectile)
  (setq projectile-cache-file (expand-file-name  ".projectile.cache" "~"))
  (projectile-global-mode +1)
  (message "..."))

(provide 'ze-projectile)
