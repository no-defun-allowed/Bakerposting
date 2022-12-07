(in-package :bakerposting)

(defun post-to-pleroma (status)
  (multiple-value-bind (response status-code)
      (drakma:http-request
       (format nil "https://~a/api/v1/statuses" (getf *config* :pleroma-domain))
       :basic-authorization (getf *config* :pleroma-basic-auth)
       :method :post
       :content-type "application/json"
       :content (jsown:to-json `(:obj ("status" . ,status))))
    (declare (ignore response))
    (assert (= status-code 200))))

(define-hook pleroma
  (bt:make-thread
   (lambda ()
     (loop
       (handler-case
           (post-to-pleroma (random-quote :length-limit 400))
         (:no-error (&rest r)
           (declare (ignore r))
           (write-line "posted to pleroma"))
         (error (e) (print e)))
       (sleep (* 2 60 60))))
   :name "Pleroma"))

