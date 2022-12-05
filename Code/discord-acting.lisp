(in-package :bakerposting)

(defvar *actors*
  (alexandria:alist-hash-table (getf *config* :actors)
                               :test #'equal))
(defvar *scripts*
  (read-from-string
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname :bakerposting "../Assets/scripts.lisp"))))

(defun run-script (s speak knot)
  (labels ((run (remaining)
             (etypecase (first remaining)
               (string
                (funcall speak (first remaining) (second remaining))
                (run (cddr remaining)))
               ((eql :again)
                (funcall knot))
               (null))))
    (run (rest s))))

;; Check that all names have webhooks ahead of time.
(mapcar (lambda (script)
          (run-script script
                      (lambda (name text)
                        (declare (ignore text))
                        (unless (gethash name *actors*)
                          (error "No webhook for actor ~A" name)))
                      (constantly nil)))
        *scripts*)
  
(defun act-to-terminal ()
  (run-script (alexandria:random-elt *scripts*)
              (lambda (name text)
                (format *debug-io* "~&~A: ~A" name text))
              #'act-to-terminal))

(defun send-actor-message (name text)
  (drakma:http-request (gethash name *actors*)
                       :method :post
                       :content-type "application/json"
                       :content (jsown:to-json `(:obj ("content" . ,text))))
  (sleep (+ 3 (random 2))))

(defun act-to-discord ()
  (run-script (alexandria:random-elt *scripts*)
              #'send-actor-message
              #'act-to-discord))

(defconstant +hour+ 3600)
(define-hook discord-acting
  (bt:make-thread
   (lambda ()
     (loop
       (handler-case (act-to-discord)
         (error (e) (print e)))
       (sleep (+ (* 2 +hour+)
                 (random +hour+)))))))
