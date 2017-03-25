(defvar ze-scss-compass/deps '(scss-mode))

;; C-c C-c to compile the buffer

(defun ze-scss-compass/init ()
  "..."
  (autoload 'scss-mode "scss-mode" "Major mode for editing sass scss files" t)
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (defcustom scss-sass-command "compass compile"
	 "Command used to compise SCSS files"
	 :group 'scss)
  
  (eval-after-load 'scss-mode
    '(progn
       (defun get-compass-root-dir ()
	 "Get root folder of the compass project relative to the buffer"
	 (locate-dominating-file buffer-file-name "config.rb"))

       (defun scss-compile ()
	 "Compile compass project associated ccurent buffer"
	 (interactive)
	 (message (get-compass-root-dir))
	 (let ((default-directory (get-compass-root-dir)))
	   (message (pwd))
	   (compile scss-sass-command))))))

(provide 'ze-scss-compass)
