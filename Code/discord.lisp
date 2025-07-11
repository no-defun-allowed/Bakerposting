(in-package :bakerposting)

(defun handle-message (bot data)
  (let ((content (jsown:val data "content"))
        (channel (jsown:val data "channel_id")))
   (flet ((reply (text)
            (freddie-laker:send-message bot channel text)))
     (when (freddie-laker:was-pinged-p bot content)
       (reply (if (search "uptime" content)
                  (uptime)
                  (search-quote content))))
     (when (search "stop with the shaking" content)
       (reply "IT'S TIME TO GET BAKING!!!!! https://youtu.be/wvGCVfX18ps?t=825"))
     (when (string= "69" content)
       (reply (format nil "lmaooooo the sex number X~{~A~}"
                      (loop repeat (+ 1 (random 5))
                            collect "D")))))))

(defvar *discord*)

(define-hook discord
  (setf *discord*
        (make-instance 'freddie-laker:bot
          :token (getf *config* :discord-token)
          :on-message 'handle-message))
  (freddie-laker:start *discord*))
