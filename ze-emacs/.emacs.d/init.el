; M-x customize-mode RET (auto-complete mode)
; C-h m -- view actions for *active* major & minor modes
; M-x describe-variable
; M-x set-variable


(when (< emacs-major-version 24)
  (error "Emacs is too old, aborting! -- patch your sh!t"))


;; Add additional directories to load from
(add-to-list 'load-path "~/.emacs.d/packages/")

(defun ze-mb->b (mb)
  "convert number in MB to B"
  (* mb 1000000))

;; trigger GC after 30MB (default is 0.76)
(setq gc-cons-threshold (ze-mb->b 30))

;; warn on opening files greater than 30MB
(setq large-file-warning-threshold (ze-mb->b 30))

(add-to-list 'load-path "~/.emacs.d/modules")
(require 'ze-pkg)

;; Can I have modules depend on (and thereby force-load) other modules?
;; Can I ensure that user-settings are applied after defaults ?

(ze-mod-load "theme") ;; load before 'base'
(ze-mod-load "base") ;; TODO - can we ensure highlighting options apply LAST? (some hook?)
(ze-mod-load "projectile")
(ze-mod-load "neotree")
(ze-mod-load "company")
(ze-mod-load "ggtags")
;(ze-mod-load "fsharp")
;(ze-mod-load "racket")
(ze-mod-load "clojure")
;(ze-mod-load "php")
;(ze-mod-load "elm")

(eval-after-load 'fsharp-mode
  (add-hook 'fsharp-mode-hook
	    (lambda ()
	      (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point))))

(ze-init)
