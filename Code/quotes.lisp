(in-package :bakerposting)

(defun remove-newlines (string)
  (substitute #\Space #\Newline string))

(defun uptime ()
  (format nil "~$ days uptime"
          (/ (get-internal-real-time)
             internal-time-units-per-second
             86400)))

(defun sentences (paragraph)
  (let* ((paragraph (remove-newlines paragraph))
         (fragments
           (mapcar (alexandria:rcurry #'svref 0)
                   (one-more-re-nightmare:all-string-matches "[¬.!?]+[.!?]?" paragraph)))
         (results '())
         (buffer (make-string-output-stream)))
    ;; We want to re-join fragments if they don't look like they could end
    ;; a sentence. This includes numbers e.g. "123.0" does not split a sentence,
    ;; and abbreviations e.g. "e.g.".
    (labels ((number-p (s) (every #'digit-char-p s))
             (short-p (s) (= (length s) 1))
             (ending-p (s)
               (let ((last-space (position #\Space s :from-end t)))
                 (and (> (length s) 20)
                      (or (null last-space)
                          (= last-space (1- (length s)))
                          (let ((last-word (subseq s (1+ last-space) (1- (length s)))))
                            (and (not (number-p last-word))
                                 (not (short-p last-word))))))))
             (flush ()
               (push (get-output-stream-string buffer) results)
               (setf buffer (make-string-output-stream))))
      (dolist (f fragments)
        (write-string f buffer)
        (when (ending-p f) (flush)))
      (flush)
      (loop for raw in results
            for result = (string-trim " " raw)
            when (plusp (length result))
              collect result))))

(defun scrape (pathname)
  (let* ((dom (plump:parse (alexandria:read-file-into-string pathname)))
         (h1 (lquery:$ dom "h1" (plump:text)))
         (title (if (plusp (length h1))
                    (aref h1 0)
                    (aref (lquery:$ dom "title" (plump:text)) 0)))
         (paragraphs (lquery:$ dom "p" (plump:text)))
         (sentences
           (remove-duplicates
            (loop for s in (reduce #'append paragraphs :key #'sentences)
                  when (< 10 (length s) 2000)
                    collect (format nil "~A~%(~A)" s title))
            :test #'equal)))
    (format *debug-io* "~&~d sentences from ~a" (length sentences) pathname)
    sentences))

(defvar *article-path*
  (merge-pathnames "*.html"
                   (asdf:system-relative-pathname :bakerposting "../Assets/Articles/")))
(defvar *quotes*
  (alexandria:mappend #'scrape (directory *article-path*)))

(defun random-quote (&key (length-limit nil))
  (let ((selected (alexandria:random-elt *quotes*)))
    (if (or (null length-limit)
            (< (length selected) length-limit))
        selected
        (random-quote :length-limit length-limit))))
