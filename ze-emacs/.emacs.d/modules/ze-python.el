(defvar ze-python/deps '(python-mode jedi-core company-jedi pyvenv))

(setq ze-py-venv-change-hook '())

;; multiple modes (python & hy) rely on knowing the venv
;; -> hence a minimal fun to set a var and run related hooks which
;;    those modes can install.
(defun ze:venv (&optional venv)
  (interactive (list (read-directory-name "venv path: ")))
  (setq ze-py-venv-path (expand-file-name venv))
  (run-hooks 'ze-py-venv-change-hook)
  (message "venv switched to '%s'" ze-py-venv-path))

(defun ze-python/init ()
  ;; autoload - only load modules in when python-mode is requested
  (autoload 'python-mode "python-mode" "Major mode for editing Python code" t)

  ;; ensure .py files trigger python-mode
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

  (eval-after-load 'python-mode
    '(progn
       (message "py mode loaded...")
       (require 'virtualenvwrapper)
       (venv-initialize-interactive-shells)

       (add-hook
	'ze-py-venv-change-hook
	(lambda ()
	  (message "py-mode-venv hook run")
	  (let ((venv-path ze-py-venv-path))
	   (setq venv-location (ze:basepath venv-path))
	   (venv-workon (ze:basename venv-path))
	   
	   ;; because virtualenvwrapper won't inform jedi.
	   ;; ------------------
           ;; jedi:environment-virtualenv (& jedi:environment-root which must be set to
	   ;; non-nil for the former var to take effect) are red herrings.
	   ;;
	   ;; The only method I've gotten to work is forcefully redefining
	   ;; server-args *AND* server-command like so to include the intended virtualenv.
	   (setq jedi:server-args
		 (list "--virtual-env" venv-path))
	   (let ((pybin (ze:join (list venv-path "bin" "python") "/")))
	     (setq jedi:server-command (list pybin jedi:server-script)))

	   ;; Conditionally (re)start the jedi server
	   (when (or (jedi:-get-servers-in-use) (eq major-mode 'python-mode))
	     (jedi:stop-server)
	     (jedi:setup)))))

       (require 'eval-in-repl)
       (require 'eval-in-repl-python)
       ;;(setq eir-jump-after-eval nil)
       ;;(setq eir-always-split-script-window t)
       (add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))))

(defun python-mode-defaults ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'python-mode-defaults)

(provide 'ze-python)
