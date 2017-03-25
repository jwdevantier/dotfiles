; M-x customize-mode RET (auto-complete mode)
; C-h m -- view actions for *active* major & minor modes
; M-x describe-variable
; M-x set-variable



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (< emacs-major-version 24)
  (error "Emacs is too old, aborting! -- patch your sh!t"))


;; Add additional directories to load from
(add-to-list 'load-path "~/.emacs.d/packages/")

;;(load "term/xterm")
;;(tty-run-terminal-initialization (selected-frame) "rxvt")

(defun ze-mb->b (mb)
  "convert number in MB to B"
  (* mb 1000000))

;; trigger GC after 30MB (default is 0.76)
(setq gc-cons-threshold (ze-mb->b 30))

;; warn on opening files greater than 30MB
(setq large-file-warning-threshold (ze-mb->b 30))

(dolist (path '("~/.emacs.d/modules" "~/.emacs.d/shared"))
  (push path load-path))
(require 'ze-pkg)

;; Can I have modules depend on (and thereby force-load) other modules?
;; Can I ensure that user-settings are applied after defaults ?

(ze-mod-load "theme") ;; load before 'base'
(ze-mod-load "base") ;; TODO - can we ensure highlighting options apply LAST? (some hook?)
(ze-mod-load "mac") ;; (IFF mac OS) - load mac-specific tweaks
(ze-mod-load "common")
(ze-mod-load "projectile")
(ze-mod-load "neotree")
(ze-mod-load "company")
(ze-mod-load "ggtags")
;(ze-mod-load "fsharp")
(ze-mod-load "racket")
(ze-mod-load "clojure")
;(ze-mod-load "php")
;(ze-mod-load "elm")
(ze-mod-load "python")
(ze-mod-load "hy")
(ze-mod-load "cl")

(eval-after-load 'fsharp-mode
  (add-hook 'fsharp-mode-hook
	    (lambda ()
	      (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point))))

(ze-init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eval-in-repl virtualenvwrapper spacemacs-theme rainbow-delimiters pyvenv python-mode projectile powerline neotree move-text hy-mode ggtags company-jedi cider base16-theme)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#fefeff"))))
 '(company-scrollbar-fg ((t (:background "#fefeff"))))
 '(company-tooltip ((t (:inherit default :background "#fefeff"))))
 '(company-tooltip-annotation ((t (:foreground "light coral"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face :foreground "light goldenrod"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))
