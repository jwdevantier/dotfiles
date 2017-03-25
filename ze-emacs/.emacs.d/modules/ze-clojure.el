(defvar ze-clojure/deps '(clojure-mode cider))

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

  (defun cider-mode-defaults ()
    (message "Cider mode defaults loading...")
    (setq cider-popup-stacktraces nil)
    (setq cider-show-error-buffer 'only-in-repl)

    ;; TODO: will trigger even in the event of cider using 'boot'.
    ;; (setq cider-cljs-lein-repl
    ;; 	  "(do (require 'figwheel-sidecar.repl-api)
    ;;            (figwheel-sidecar.repl-api/start-figwheel!)
    ;;            (figwheel-sidecar.repl-api/cljs-repl))")

    (ze:bind cider-mode-map
	     '(;;Documentation - duplicate binds to mimic CL setup
	       ("H-h f" . cider-doc)
	       ("H-h s" . cider-doc)
	       ("H-h a" . cider-apropos)
	       ("H-h h" . clojure-hyperspec)

	       ;;REPL - amalgation of categories

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

	       ;;macro-expansion tools
	       ("H-r m" . cider-macroexpand-1)
	       ("H-r M" . cider-macroexpand-all))))
  (add-hook 'cider-mode-hook 'cider-mode-defaults))

(provide 'ze-clojure)
