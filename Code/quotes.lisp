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

(defstruct document title word-count words sentences)
(defmethod print-object ((d document) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (write-string (document-title d) stream)))

(defun words (sentences)
  (let ((words (make-hash-table :test 'equal))
        (word-count 0))
  (dolist (s sentences)
    (let ((text (sentence-text s)))
      (one-more-re-nightmare:do-matches ((start end) "[A-Za-z]+" text)
        (let ((stem (stem:stem (string-downcase (subseq text start end)))))
          (incf word-count)
          (incf (gethash stem words 0))))))
    (values words word-count)))

(defun compute-idf (documents)
  (let ((idf (make-hash-table :test 'equal)))
    (flet ((inverse (n)
             (log (/ (1+ (length documents)) (1+ n)))))
      (dolist (d documents idf)
        (maphash (lambda (word count)
                   (declare (ignore count))
                   (unless (gethash word idf)
                     (setf (gethash word idf)
                           (inverse
                            (loop for d in documents
                                  count (gethash word (document-words d)))))))
                 (document-words d))))))

(defstruct sentence text source)
(defun render-quote (sentence)
  (format nil "~A (~A)" (sentence-text sentence) (sentence-source sentence)))

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
                    collect (make-sentence :text s :source title))
            :test #'equal
            :key #'sentence-text)))
    (format *debug-io* "~&~d sentences from ~a" (length sentences) pathname)
    (multiple-value-bind (words word-count)
        (words sentences)
      (make-document
       :title title
       :word-count word-count
       :words words
       :sentences sentences))))

(defvar *article-path*
  (merge-pathnames "*.html"
                   (asdf:system-relative-pathname :bakerposting "../Assets/Articles/")))
(defvar *documents* (mapcar #'scrape (directory *article-path*)))
(defvar *idf* (compute-idf *documents*))
(defvar *quotes* (alexandria:mappend #'document-sentences *documents*))

(defun random-quote (&key (length-limit nil) (sentences *quotes*))
  (let ((selected (render-quote (alexandria:random-elt sentences))))
    (if (or (null length-limit)
            (< (length selected) length-limit))
        selected
        (random-quote :length-limit length-limit :sentences sentences))))

(defun weighted-random (alist)
  (let* ((total (reduce #'+ alist :key #'cdr))
         (point (random total)))
    (dolist (pair alist)
      (decf point (cdr pair))
      (unless (plusp point) (return (car pair))))))

(defun search-quote (query &key (length-limit nil))
  ;; tf-idf. I think. Maybe. It works well enough.
  (let* ((words (alexandria:hash-table-alist (words (list (make-sentence :text query)))))
         (words (loop for (w . c) in words
                      collect (cons w (* c c))))
         (document-magnitudes
           (loop for d in *documents*
                 collect (sqrt (loop for w being the hash-keys of (document-words d)
                                       using (hash-value c)
                                     sum (expt (* (/ c (document-word-count d)) (gethash w *idf*)) 2)))))
         (similarities
           (loop for document in *documents*
                 for magnitude in document-magnitudes
                 collect (cons document
                               (loop for (word . count) in words
                                     for idf = (gethash word *idf* 0)
                                     for tf = (/ (gethash word (document-words document) 0)
                                                 (document-word-count document))
                                     sum (* idf count (sqrt tf) (/ magnitude)))))))
    (setf similarities (sort similarities #'> :key #'cdr))
    (if (zerop (cdr (first similarities)))
        (random-quote :length-limit length-limit)
        (random-quote
         :length-limit length-limit
         :sentences (document-sentences (weighted-random similarities))))))
