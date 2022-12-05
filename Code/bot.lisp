(in-package :bakerposting)

(defvar *start-hooks* '())
(defmacro define-hook (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (pushnew ',name *start-hooks*)))

(defun start ()
  (mapc #'funcall *start-hooks*))

(defvar *config*
  (read-from-string
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname :bakerposting "../config.lisp"))))
