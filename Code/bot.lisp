(in-package :bakerposting)

(defvar *start-hooks* '())
(defmacro define-hook (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (pushnew ',name *start-hooks*)))

(defun start ()
  (mapc #'funcall *start-hooks*))

(defvar *config*)
(defvar *config-hooks* '())
(defmacro define-config-load-hook (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (pushnew ',name *config-hooks*)))

(defun load-config ()
  (setf *config*
        (read-from-string
         (alexandria:read-file-into-string
          (asdf:system-relative-pathname :bakerposting "../config.lisp"))))
  (mapc #'funcall *config-hooks*)
  (values))
