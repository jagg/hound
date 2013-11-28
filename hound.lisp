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
