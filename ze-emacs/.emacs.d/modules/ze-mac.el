(defvar ze-mac/deps '())

(defun ze-mac/do-init ()
  (message "ze-mac: mac OS detected - loading settings")

  ;; Make HOME & END move to beginning & end of line respectively
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)
  
  ;; Swap Option (Alt) & CMD (Meta)

  (setq mac-option-key-is-meta t
	mac-command-key-is-meta nil
	mac-command-modifier 'meta
	mac-option-modifier nil)

  ;; Two last settings above + this enables {} and []
  (setq x-select-enable-clipboard t))

(defun ze-mac/init ()
  (if (string-equal system-type "darwin")
      (ze-mac/do-init)))

(provide 'ze-mac)
