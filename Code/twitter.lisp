(in-package :bakerposting)

(multiple-value-setq (chirp:*oauth-api-key* chirp:*oauth-api-secret*
                      chirp:*oauth-access-token* chirp:*oauth-access-secret*)
  ;; These are the API thingies for chirp. Perhaps I should
  ;; make my own application, but can't be bothered to give
  ;; Twitter my phone number.
  (values "D1pMCK17gI10bQ6orBPS0w"
          "BfkvKNRRMoBPkEtDYAAOPW4s2G9U8Z7u3KAf0dBUA"
          (getf *config* :twitter-access-token)
          (getf *config* :twitter-access-secret)))

(define-hook twitter
  (bt:make-thread
   (lambda ()
     (loop
       (handler-case
           (chirp:statuses/update (random-quote :length-limit 280))
         (:no-error (&rest r)
           (declare (ignore r))
           (write-line "posted to twitter"))
         (error (e) (print e)))
       (sleep (* 2 60 60))))
   :name "Twitter"))

