(defvar ze-neotree/deps '(neotree))

(defun ze-neotree/init ()
 (global-set-key (kbd "C-b") 'neotree-toggle))

(provide 'ze-neotree)
