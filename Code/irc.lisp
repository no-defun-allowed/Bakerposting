(in-package :bakerposting)

(defvar *irc*)

(defun irc-reply (message)
  (destructuring-bind (channel text)
      (irc:arguments message)
    (when (or (search "pony" text) (search "ponies" text))
      (irc:notice *irc* channel "OMG!! Ponies!!!"))
    (when (and (search "Bakerposting" text)
               (not (search "'Bakerposting" text))
               (not (search "(quote Bakerposting)" text)))
      (irc:notice *irc*
                  channel
                  (if (search "uptime" text)
                      (uptime)
                      (remove-newlines (random-quote :length-limit 300))))))
  t)

(defun reconnect ()
  (declare (ignore message))
  (setf *irc*
        (irc:connect
         :server "irc.libera.chat"
         :nickname "Bakerposting"
         :password (getf *config* :irc-password)))
  (dolist (c (getf *config* :irc-channels))
    (irc:join *irc* c))
  (irc:add-hook *irc* 'irc:irc-privmsg-message 'irc-reply)
  (irc:add-hook *irc* 'irc:irc-quit-message
                (lambda (m)
                  (declare (ignore m))
                  (sleep 3)
                  (reconnect)))
  (bt:make-thread (lambda () (irc:read-message-loop *irc*))
                  :name "IRC listener"))

(define-hook irc (reconnect))
