(defvar ze-base/deps '(move-text eval-in-repl xterm-color exec-path-from-shell helm-ag paredit))
(require 'ansi-color)

(defun ze-base/enable-paredit* ()
  ;;enable paredit support for emacs lisp
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (dolist (mode '(emacs-lisp-mode-hook
		  ielm-mode-hook
		  lisp-mode-hook
		  lisp-interaction-mode-hook
		  scheme-mode-hook))
    (add-hook mode #'enable-paredit-mode))
  (let ((-ze-paredit-wrap (lambda ()
			    ()))))
  (add-hook 'paredit-mode-hook
	    (lambda ()
	      (->>
	       '(("M-<down>" . nil)
		 ("M-<up>" . nil)
		 ("C-M-k" . paredit-kill-region)
		 ;; Wrap SEXP
		 ("H-w (" . paredit-wrap-round)
		 ("H-w [" . paredit-wrap-square)
		 ("H-w {" . paredit-wrap-curly)
		 ;; H-s + <left>/<right> --> splurp, + M to barf
		 ("H-s <left>" . paredit-backward-slurp-sexp)
		 ("H-s <right>" . paredit-forward-slurp-sexp)
		 ("H-s M-<left>" . paredit-backward-barf-sexp)
		 ("H-s M-<right>" . paredit-forward-barf-sexp)
		 ;; H-s + <up>/<down>/SPC  --> splice
		 ("H-s <up>" . paredit-splice-sexp-killing-forward)
		 ("H-s <down>" . paredit-splice-sexp-killing-backward)
		 ("H-s SPC" . paredit-splice-sexp)
		 ;; Navigation
		 ("H-<right>" . paredit-forward)
		 ("H-<left>" . paredit-backward)
		 ("H-<down>" . paredit-forward-down)
		 ("H-M-<down>" . paredit-backward-down)
		 ("H-<up>" . paredit-backward-up)
		 ("H-M-<up>" . paredit-forward-up)
		 )
	       (ze:bind paredit-mode-map)))))

(defun ze-base/init ()
  (message "ze-base/init called")

  ;;(require 'xterm-color)
  ;; comint install
  ;;(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  ;;       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
  ;;       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))
  ;;(setenv "TERM" "xterm-256color")

  ;; enable paredit mode for lisp's
  (ze-base/enable-paredit*)

  ;; Tweak Emacs UI
  ;; --------------
  (if window-system                                ; hide tool bar (only triggers in GUI mode)
      (tool-bar-mode -1))
  (menu-bar-mode -1)                               ;don't show the menubar
  (setq inhibit-startup-screen t)                  ;hide welcome screen

  ;; Misc. tweaks
  ;; automatically reload files when they're changed on disk.
  (global-auto-revert-mode t)
  ;; grave accent + tilde required this (2x key + space)
  (require 'iso-transl)

  ;; IDO tweaks
  ;; ----------
  (require 'ido)
  (ido-mode t)
  (setq ido-create-new-buffer 'always)

  ;; Prompt-related tweaks
  ;; ---------------------
  (setq confirm-nonexistent-file-or-buffer nil) ; don't prompt when making new buffers/files
  (fset 'yes-or-no-p 'y-or-n-p)                 ;changes yes/no prompts to y/n
  
  ;;C-Z would suspend emacs & royally F-up my day - disable it!
  (global-unset-key (kbd "C-z"))

  ;; keybinds for manipulating regions
  (global-set-key (kbd "C-c m") 'mark-whole-buffer)
  (global-set-key (kbd "C-c i") 'indent-region)
  (global-set-key (kbd "C-c c") 'comment-region)
  (global-set-key (kbd "C-c u") 'uncomment-region)

  (global-set-key (kbd "C-SPC") 'company-complete)

  ;; keybinds for moving between buffers
  ;;Map <Alt>+{arrow-key} to navigate between buffers
  (global-set-key [M-up] 'windmove-up)
  (global-set-key [M-down] 'windmove-down)
  (global-set-key [M-left] 'windmove-left)
  (global-set-key [M-right] 'windmove-right)

  ;; Ensure Emacs PATH var corresponds to shell's
  (exec-path-from-shell-initialize)

  ;; Highlighting settings
  ;; ---------------------
  (show-paren-mode 1)                                  ;highlight matching parentheses
  ;; Line highlighting
  (global-linum-mode t)                                ;line numbers
  (global-hl-line-mode t)                              ;highlight current line
  (set-face-attribute hl-line-face nil :underline nil) ;don't underline highlight

  ;; Highlight whitespace
  ;; to enable in x: (add-hook 'x-mode-hook 'whitespace-mode)
  (require 'whitespace)
  (setq whitespace-style '(spaces tabs space-mark tab-mark))

  ;; Buffer / editing tweaks
  ;; -----------------------
  ;;; [Move text]
  ;; allows moving lines/regions up and down
  ;; [M-S-up] => move-text-up
  ;; [M-S-down] => move-text-down
  (global-set-key [C-up] 'move-text-up)
  (global-set-key [C-down] 'move-text-down))

(provide 'ze-base)
