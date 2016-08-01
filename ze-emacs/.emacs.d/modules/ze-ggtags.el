(defvar ze-ggtags/deps '(ggtags))

;; [How to use]
;; ------------
;; Simply add (ggtags-mode 1) to the hook of whichever
;; major mode in which you'd like ggtags support.
;; (Keep in mind, M-\. overrides some keybinds (e.g. intellisense in FSharp-Mode)

;;; [ggtags config]
;; purpose: code navigation (tagging)

; M- .	-- go to definition
; M- ,	-- abort search (goes back to where the search started, NESTED!!)
; M- -	-- find references to identifier
;  (going through matches)
;    M-,<   -- change to first match
;    M-,>   -- change to last match
;    M-,n   -- change to next match
;    M-,p   -- change to previous match

;Next 2 functions collectively allow incremental GTAGS updating
(defun ggtags-root-dir ()
  "returns GTAGS root dir or nil if it doesn't exist"
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
	(buffer-substring (point-min) (1- (point-max))))))

(defun ggtags-update-hook ()
  "if GTAGS root dir has been found, do incremental
  updates of tag files when executed (install as
  'after-save-hook' when 'ggtags-mode'!)"
  (call-process "global" nil nil nil "-u"))

(defun ze-ggtags/init ()
  ;; autoload - only load in module once something else references it
  ;; (in this case - it should be triggrered by a major mode's (e.g. Java's)
  ;; expect others to load this
  ;; hence attach everything to the ggtags-mode-hook
  (defun ze-ggtags-mode-defaults ()
    "Default settings for ggtags"
    ;; enable incremental updating of tag files (on file save)
    (add-hook 'after-save-hook #'ggtags-update-hook)

    (define-key ggtags-mode-map (kbd "M-\-") 'ggtags-find-reference)
    (define-key ggtags-mode-map (kbd "M-\,") 'pop-tag-mark))

  (setq ze-ggtags-mode-hook 'ze-ggtags-mode-defaults)
  (add-hook 'ggtags-mode-hook
	    (lambda ()
	      (run-hooks 'ze-ggtags-mode-hook))))

(provide 'ze-ggtags)
