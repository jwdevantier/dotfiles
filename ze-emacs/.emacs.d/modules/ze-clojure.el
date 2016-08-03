(defvar ze-clojure/deps '(clojure-mode cider))

(defun ze-clojure/init ()
  ;; autoload - only load modules in when clojure-mode is requested
  (autoload 'clojure-mode "clojure-mode" "Major mode for editing Clojure code" t)

  ;; ensure .clj/.cljc files trigger clojure-mode (and .cljs trigger clojure-script)
  (add-to-list 'auto-mode-alist '("\\.clj[c]?$" . clojure-mode))
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
               (figwheel-sidecar.repl-api/cljs-repl))"))
  (add-hook 'cider-mode-hook 'cider-mode-defaults))

(provide 'ze-clojure)
