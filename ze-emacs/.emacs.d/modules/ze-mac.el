(defvar ze-mac/deps '())

(defun ze-mac/do-init ()
  (message "ze-mac: mac OS detected - loading settings")

  ;; Make HOME & END move to beginning & end of line respectively
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)
  
  ;; Swap Option (Alt) & CMD (Meta)
  ;; & set 'fn' key to act as hyper
  ;; (To get caps=>hyper:
  ;;  1) In karabiner elements (install, then reboot)
  ;;     - In "simple modifications", From(caps_lock), To(fn)
  ;;  2) System Preferences, Keyboard:
  ;;     - In "shortcuts", clear all for every section
  ;;     - In "keyboard" tab, "Modifier keys..." btn: "Caps Lock"=>No Action, for all KB's
  (setq mac-option-key-is-meta t
	mac-command-key-is-meta nil
	mac-command-modifier 'meta
	mac-option-modifier nil
	mac-function-modifier 'hyper)

  ;; Two last settings above + this enables {} and []
  (setq x-select-enable-clipboard t))

(defun ze-mac/init ()
  (if (string-equal system-type "darwin")
      (ze-mac/do-init)))

(provide 'ze-mac)
