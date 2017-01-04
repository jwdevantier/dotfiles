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
       (add-hook 'clojure-mode-hook 'cider-mode)))

  (defun cider-mode-defaults ()
    (setq cider-popup-stacktraces nil)
    (setq cider-show-error-buffer 'only-in-repl)
    (setq cider-cljs-lein-repl
	  "(do (require 'figwheel-sidecar.repl-api)
               (figwheel-sidecar.repl-api/start-figwheel!)
               (figwheel-sidecar.repl-api/cljs-repl))")

    (dolist (bind '(;;Documentation
		    ("H-h f" . cider-doc)
		    ("H-h s" . cider-doc)
		    ("H-h a" . cider-apropos)
		    ;;Can we get hyperspec lookup..??

		    ;; REPL - amalgation of categories
		    ("H-r r" . cider-jack-in)
		    ("H-r R" . cider-jack-in-clojurescript)
		    ("H-r k" . cider-load-file)
		    ("H-r e" . cider-eval-last-sexp)
		    ("H-r s" . cider-refresh)


		    ("H-r d" . cider-eval-defun-at-point)
		    ("H-r t" . cider-toggle-trace-var)
		    ("H-r T" . cider-toggle-trace-ns);; not same as slime(untrace) but useful


		    ("H-r p" . cider-repl-set-ns) ;; - set REPL NS


		    ("H-r m" . cider-macroexpand-1)
		    ("H-r M" . cider-macroexpand-all)))))
  (add-hook 'cider-mode-hook 'cider-mode-defaults))

(provide 'ze-clojure)
