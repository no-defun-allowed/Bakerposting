(in-package :bakerposting)

(maiden:define-consumer baking () ())

(defun reply-with-notice (e message)
  (maiden-irc-events:notice
   (maiden-client-entities:client e)
   (maiden:name
    (maiden-client-entities:channel e))
   message))

(maiden:define-handler (baking maybe-reply maiden-client-entities:message-event) (c e)
  (let ((text (maiden-client-entities:message e)))
    (when (or (search "pony" text) (search "ponies" text))
      (reply-with-notice e "OMG!! Ponies!!!"))
    (when (and (search "Bakerposting" text)
               (not (search "'Bakerposting" text))
               (not (search "(quote Bakerposting)" text)))
      (reply-with-notice
       e
       (if (search "uptime" text)
           (uptime)
           (remove-newlines (search-quote text :length-limit 300)))))))

(defvar *core*)

(define-hook irc
  (setf *core*
        (maiden:make-core
         `(:maiden-irc
           :nickname "Bakerposting"
           :password ,(getf *config* :irc-password)
           :host "irc.libera.chat"
           :channels ,(getf *config* :irc-channels))
         'baking)))
