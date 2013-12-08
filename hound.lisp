;;;; hound.lisp

(in-package #:hound)


(defgeneric add-document (index text)
  (:documentation "Adds a document to the index"))

(defgeneric query (index query-string)
  (:documentation "Query the index and retrieve documentes matching the query"))

(defstruct document
  (id (error "Id is mandatory"))
  (contents nil))

(defun simple-tokenizer (text)
  (split-sequence:split-sequence #\Space text))

(defclass in-memory-index ()
  ((cache :initarg :cache :accessor cache :initform (make-hash-table))
   (term-dictionary :initarg :term-dic :accessor term-dic :initform (trie:make-trie))
   (id-counter :accessor id-counter :initform 0)
   (analyzer :accessor analyzer :initform #'simple-tokenizer)))

(defun next-id (memory-index)
  (let ((counter (id-counter memory-index)))
    (setf (id-counter memory-index) (+ 1 counter))
    counter))

(defmethod add-document ((index in-memory-index) text) 
  (with-accessors ((cache cache) (term-dic term-dic)) index
    (let* ((id (next-id index))
	   (terms (funcall (analyzer index) text))
	   (document (make-document :id id :contents text)))
      (flet ((add (term)
	       (let ((postings (trie:get-output term-dic term)))
		 (if postings (trie:set-output term-dic term (postings:pst-add postings id))
		     (trie:add-term term-dic term 
				    (postings:pst-add (postings:make-linked-pst) id))))))
	(map 'list #'add terms)
	(setf (gethash id cache) document)))))


;; Query language


(defun qcombine (index op &rest terms)
  (reduce (lambda (p1 p2) 
	    (funcall op p1 p2)) 
	  (map 'list 
	       (lambda (term) 
		 (cond 
		   ((postings:posting-p term) term)
		   (t (or (trie:get-output (term-dic index) term) (postings:make-linked-pst)))))
	       terms)))

(defmacro qand (index &rest terms)
  "Takes a list of term and returns the doc-ids that match them all"
  `(qcombine ,index #'postings:pst-intersection ,@terms))

(defmacro qor (index &rest terms)
  "Takes a list of term and returns the doc-ids that match any of them"
  `(qcombine ,index #'postings:pst-union ,@terms))

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
  (map 'list 
       (lambda (doc-id) (gethash doc-id (cache index))) 
       (when posting (postings:get-list posting))))
