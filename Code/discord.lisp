(in-package :bakerposting)
(lispcord:defbot *baker* (getf *config* :discord-token))

(lispcord:add-event-handler
 :on-message-create
 (lambda (m)
   (flet ((reply (text)
            (lispcord:reply m text *baker*)))
     (when (lispcord:commandp m *baker*)
       (reply (if (search "uptime" (lispcord.classes:content m))
                  (uptime)
                  (random-quote))))
     (when (search "stop with the shaking" (lispcord.classes:content m))
       (reply "IT'S TIME TO GET BAKING!!!!! https://youtu.be/wvGCVfX18ps?t=825"))
     (when (string= "69" (lispcord.classes:content m))
       (reply (format nil "lmaooooo the sex number X~{~A~}"
                      (loop repeat (+ 1 (random 5))
                            collect "D")))))))

(define-hook discord (lispcord:connect *baker*))
