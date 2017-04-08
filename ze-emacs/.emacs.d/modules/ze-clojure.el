(defvar ze-clojure/deps '(clojure-mode cider helm-cider))

(defun ze-clojure/init ()
  ;; autoload - only load modules in when clojure-mode is requested
  (autoload 'clojure-mode "clojure-mode" "Major mode for editing Clojure code" t)

  ;; trigger the right clojure/clojure common/clojurescript modes
  (add-to-list 'auto-mode-alist '("\\.clj?$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc?$" . clojurec-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

  (eval-after-load 'clojure-mode
    '(progn
       ;; triger cider-mode when entering clojure-mode
       (add-hook 'clojure-mode-hook 'cider-mode)

       (defun clojure-hyperspec ()
	 "Function to lookup a given query using clojuredocs.
         (This functionality mimics the CL hyperspec lookup functionality in Emacs)"
	 (interactive)
	 (ze:browser-jump
	  (lambda (search-term)
	    (concat "https://clojuredocs.org/search?q=" search-term))))))

  ;; keybinds common to edit & REPL buffers
  (setq ze-clojure-common-keybinds
	  '(("H-h h" . clojure-hyperspec)))

  (defun cider-repl-mode-defaults ()
    (ze:bind cider-repl-mode-map
	     (append ze-clojure-common-keybinds
		     '(("H-r TAB" . cider-switch-to-last-clojure-buffer)))))

  (defun cider-mode-defaults ()
    (message "Cider mode defaults loading...")
    (setq cider-popup-stacktraces nil)
    (setq cider-show-error-buffer 'only-in-repl)

    ;; TODO: will trigger even in the event of cider using 'boot'.
    ;; (setq cider-cljs-lein-repl
    ;; 	  "(do (require 'figwheel-sidecar.repl-api)
    ;;            (figwheel-sidecar.repl-api/start-figwheel!)
    ;;            (figwheel-sidecar.repl-api/cljs-repl))")

    (ze:bind
     cider-mode-map
     (append ze-clojure-common-keybinds
	     '(;;Documentation - duplicate binds to mimic CL setup
	       ("H-h f" . cider-doc)
	       ("H-h s" . cider-doc)
					; search among defs in loaded NS's
	       ("H-h a" . cider-apropos)
					; search among loaded NS's
	       ("H-h n" . cider-browse-ns)
					; consult clojuredocs on selection/word at point
	       ("H-h h" . clojure-hyperspec)

	       ;;REPL - amalgation of categories
	       ("H-r TAB" . cider-switch-to-repl-buffer)

	       ;; load into lein/boot clojure/clojurescript project
	       ("H-r r" . cider-jack-in)
	       ("H-r R" . cider-jack-in-clojurescript)

	       ;; load file/sexp into process
	       ("H-r k" . cider-load-file)
	       ("H-r e" . cider-eval-last-sexp)
	       ;; eval the current top-level form
	       ("H-r d" . cider-eval-defun-at-point)
	       ;; sync (saved) file changes into running process
	       ("H-r s" . cider-refresh)

	       ("H-r t" . cider-toggle-trace-var)
	       ("H-r T" . cider-toggle-trace-ns)

	       ("H-r p" . cider-repl-set-ns) ;; - set REPL NS

	       ;; run all tests associated project
	       ("H-t p" . cider-test-run-project-tests)
	       ;; run test @ point (mimic eval defun @ point, hence 'd')
	       ("H-t d" . cider-test-run-test)
	       ;; Run tests for current NS (package)
	       ("H-t p" . cider-test-run-ns-tests)
	       ;; clear highlights in buffer from last test run
	       ("H-t c" . cider-test-clear-highlights)

	       ;;macro-expansion tools
	       ("H-r m" . cider-macroexpand-1)
	       ("H-r M" . cider-macroexpand-all))))

  (helm-cider-mode 1))
  (add-hook 'cider-mode-hook 'cider-mode-defaults)
  (add-hook 'cider-repl-mode-hook 'cider-repl-mode-defaults))

(provide 'ze-clojure)
