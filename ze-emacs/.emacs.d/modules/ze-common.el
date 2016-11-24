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

(cl-defun ze:join (lst &optional (join-str nil))
  ;; concat list of strings (lst).
  ;; optionally, a join-string can be defined to be interposed between
  ;; each string in the list.
  (mapconcat 'identity lst join-str))

(provide 'ze-common)
