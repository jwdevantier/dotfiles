
(defvar ze-theme/deps '(spacemacs-theme powerline))

(defun ze-theme/init ()
  ;; (have to run before highlight font customization or else the theme
  ;; will override them.
  (load-theme 'spacemacs-light t)

  ;; 1) M-x 'set-default-font'  -- TAB twice to get a list
  ;; 2) scroll through, try each until you find something you like
  ;; 3) M-x 'describe-font' to read back the selection, copy this name
  (set-default-font "Source Code Pro Semibold-10")

  (require 'powerline)
  (powerline-default-theme))

(provide 'ze-theme)
