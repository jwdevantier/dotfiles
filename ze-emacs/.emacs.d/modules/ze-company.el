(defvar ze-company/deps '(company))

(defun ze-company/init ()
  (require 'company)

  (defun ze-company-mode-defaults ()
    (global-company-mode)
    (require 'color)

    ;;Tweak colours a bit
    ;; To tweak
    ;; 1) M-x 'customize-face'
    ;; 2) write 'company-'  then hit TAB repeatedly until a list shows
    ;; 3) complete the minibuffer entry to fit the name of the face you wish to edit
    ;; 4) customize the face to your heart's content
    ;;    [if the face is inherited, you can still hit 'Show all attributes' to
    ;;    find the aspects you wish to customize]
    ;;
    ;; 5) hit 'apply', tab to a buffer, test
    ;; 6) when happy, hit 'apply and save' look in .emacs/init.el in the bottom,
    ;;    fish out the new entry and add above
    
    ;; this is a quick starting setup for dark themes
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 8)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 15)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face :foreground "light goldenrod"))))
       `(company-tooltip-annotation ((t (:foreground "light coral")))))))

  (setq ze-company-mode-hook 'ze-company-mode-defaults)
  (add-hook 'after-init-hook
	    (lambda ()
	      (run-hooks 'ze-company-mode-hook))))

(provide 'ze-company)
