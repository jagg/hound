;;;; hound.lisp

(in-package #:hound)

;; http://java.dzone.com/articles/algorithm-week-graphs-and?utm_source=twitterfeed&utm_medium=twitter&utm_campaign=Feed%3A+javalobby%2Ffrontpage+%28Javalobby+%2F+Java+Zone%29


(defgeneric add-document (index text)
  (:documentation "Adds a document to the index"))

(defgeneric query (index query-string)
  (:documentation "Query the index and retrieve documentes matching the query"))

(defstruct document
  (id (error "Id is mandatory"))
  (contents nil))


(defclass in-memory-index ()
  ((cache :initarg :cache :accessor cache :initform (make-hash-table))
   (term-dictionary :initarg :term-dic :accessor term-dic :initform (trie:make-trie))
   (id-counter :accessor id-counter :initform 0)))

(defun next-id (memory-index)
  (let ((counter (id-counter memory-index)))
    (setf (id-counter memory-index) (+ 1 counter))
    counter))

(defmethod add-document ((index in-memory-index) text) 
  (with-accessors ((cache cache) (term-dic term-dic)) index
    (let* ((id (next-id index))
	   (terms (split-sequence:split-sequence #\Space text))
	   (document (make-document :id id :contents text)))
      (flet ((add (term)
	       (let ((postings (trie:get-output term-dic term)))
		 (if postings (trie:set-output term-dic term (postings:pst-add postings id))
		     (trie:add-term term-dic term 
				    (postings:pst-add (postings:make-linked-pst) id))))))
	(map 'list #'add terms)
	(setf (gethash id cache) document)))))


;; Query language

(defun qand (index &rest terms)
  "Takes a list of term and returns the doc-ids that match them all"
  (reduce (lambda (p1 p2) 
	    (when (and (not (null p1)) (not (null p2))) 
	      #'postings:pst-intersection p1 p2)) 
	  (map 'list 
	       (lambda (term) 
		 (cond 
		   ((postings:posting-p term) term)
		   (t (trie:get-output (term-dic index) term))))
	       terms)))

(defun qwildcard (index prefix)
  "Returns all the IDs for the documents that contain a term that starts with prefix"
  (reduce (lambda (p1 p2) 
	    (cond
	      ((and (not (null p1)) (not (null p2))) (postings:pst-union p1 p2))
	      ((null p1) p2)
	      (t p1)))
	  (trie:wild-card-output (term-dic index) prefix)))

(defun retrieve (index posting)
  "Retrieves the documents associated to this posting list"
  (map 'list (lambda (doc-id) (gethash doc-id (cache index))) (postings:get-list posting)))
