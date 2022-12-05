(in-package :bakerposting)
(lispcord:defbot *baker* (getf *config* :discord-token))

(lispcord:add-event-handler
 :on-message-create
 (lambda (m)
   (when (lispcord:commandp m)
     (lispcord:reply m (if (search "uptime" (lispcord.classes:content m))
                           (uptime)
                           (random-quote))))
   (when (search "stop with the shaking" (lispcord.classes:content m))
     (lispcord:reply m "IT'S TIME TO GET BAKING!!!!! https://youtu.be/wvGCVfX18ps?t=825"))
   (when (string= "69" (lispcord.classes:content m))
     (lispcord:reply m (format nil "lmaooooo the sex number X~{~A~}"
                               (loop repeat (+ 1 (random 5))
                                     collect "D"))))))

(define-hook discord (lispcord:connect *baker*))
