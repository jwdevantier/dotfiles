(defvar ze-racket/deps '(racket-mode))

(defun ze-racket/init ()
  (autoload 'racket-mode "racket-mode" "Major mode for editing Racket code" t)
  (add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))

  (eval-after-load 'racket-mode
    '(progn
       (defun ze-racket-mode-defaults ()
	 "Default racket settings"
	 (let ((racket-home "/opt/racket"))
	   (let ((racket-bin (mapconcat 'identity `(,racket-home "bin" "racket") "/")))
	     (setq racket-racket-program racket-bin))
	   (let ((raco-bin (mapconcat 'identity `(,racket-home "bin" "raco") "/")))
	     (setq racket-raco-program raco-bin))))

       (setq ze-racket-mode-hook 'ze-racket-mode-defaults)
       (add-hook 'racket-mode-hook
		 (lambda () (run-hooks 'ze-racket-mode-hook))))))

(provide 'ze-racket)
