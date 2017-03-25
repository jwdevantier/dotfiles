(defvar ze-common/deps '())
(require 'cl-lib)

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
			   (cdr form))))))

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defun ze-common/init ()
  (message "common loaded")
  nil)

(defun ze:basename (n)
  (file-name-nondirectory
   (directory-file-name n)))

(defun ze:dirname (n)
  (file-name-directory n))

(defun ze:basepath (n)
  ;; directory-file-name strips trailing slash if present
  ;; => file-name-directory strips last segment of path
  (file-name-directory
   (directory-file-name n)))

(defun ze:bind (mode-map bindings)
  "Bind keys to mode.
Takes a mode map (e.g. slime-mode-map) and a list of dotted pairs.
The first entry of a pair is the keyboard sequence expressed as a string,
the second is the corresponding function to invoke.

(e.g. '((\"TAB\" . 'slime-indent-and-complete-symbol))

NOTE: to clear a keybind, pass nil as the second value of the pair"
  (dolist (binding bindings)
    (define-key mode-map (kbd (car binding)) (cdr binding))))

(defun ze:browser-jump (build-url-fn)
  "Given a function to build the URL from a search term, open the user's browser.

  Arguments:
  * build-url-fn - function taking one argument, the word/search query"
  (let ((search-term (if (and transient-mark-mode mark-active)
			 (buffer-substring-no-properties
			  (region-beginning) (region-end))
		       (thing-at-point 'symbol))))
    (browse-url (funcall build-url-fn search-term))))

(cl-defun ze:join (lst &optional (join-str nil))
  ;; concat list of strings (lst).
  ;; optionally, a join-string can be defined to be interposed between
  ;; each string in the list.
  (mapconcat 'identity lst join-str))

(provide 'ze-common)
