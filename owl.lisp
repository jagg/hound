
(in-package #:owl)

(defvar *interactive* t)

(define-condition failed-assert (error)
  ((msg :initarg :msg :reader msg)))

(defun check (val1 val2 &optional (eq-fn #'equal))
  (if (funcall (or eq-fn #'equal) val1 val2) t
      (error 'failed-assert :msg (format nil "Expected ~a but found ~a" val1 val2))))

(defun check-property (val prop-fn)
  (if (funcall prop-fn val) t
      (error 'failed-assert :msg "Property doesn't hold")))

(defmacro report (test)
  `(restart-case
       (progn
	 (funcall ,test)
	 (format t "OK ... ~a~%"  ',test)
	 t)
     (ignore ()
       :report "Continue with the test"
       (progn
	 (format t "FAIL ... ~a~%" ',test)
	 nil))))

(defmacro defsuite (name doc &body tests)
  `(defun ,name ()
     ,doc
     (format t "~a~%" ,doc)
     (handler-bind
	 ((failed-assert #'(lambda (c) 
			     (declare (ignore c))
			     (when (not *interactive*) (invoke-restart 'ignore)))))
       (format t "Test result: ~:[FAIL~;OK~]~%" 
	       (every #'identity
		      (list
		       ,@(loop for test in tests collect `(report ,test))))))))


;; Test examples 

(defsuite posting-tests
  "Tests for basic operations with the posting list"
  #'test-remove
  #'test-add
  #'test-union-intersection)

(defun test-union-intersection ()
  (let ((ps1 (postings:make-linked-pst (list 0 1 3 5 10)))
	(ps2 (postings:make-linked-pst (list 0 2 4 6 10))))
    (flet ((union/intersection (l1 l2)
	      (check (postings:get-list (postings:pst-intersection l1 l2)) '(0 10))
	      (check (postings:get-list (postings:pst-union l1 l2)) '(0 1 2 3 4 5 6 10)))
	   (integrity ()
	     (check (postings:get-list ps1) '(0 1 3 5 10))
	     (check (postings:get-list ps2) '(0 2 4 6 10))))
      (union/intersection ps1 ps2)
      (integrity)
      (union/intersection ps2 ps1)
      (integrity))))



(defun test-add ()
  (let ((pst (postings:make-linked-pst)))
    (postings:pst-add pst 1)
    (check (postings:get-list pst) '(1))
    (postings:pst-add pst 5)
    (postings:pst-add pst 3)
    (check-property (postings:get-list pst) #'sorted-p)))

(defun test-remove ()
  (let ((pst (postings:make-linked-pst)))
    (postings:pst-add pst 1)
    (postings:pst-add pst 5)
    (postings:pst-add pst 3)
    (check-property (postings:get-list pst) #'sorted-p)
    (postings:pst-remove pst 3)
    (check (postings:get-list pst) '(1 5))
    (check-property (postings:get-list pst) #'sorted-p)
    (postings:pst-remove pst 1)
    (check (postings:get-list pst) '(5))
    (check-property (postings:get-list pst) #'sorted-p)
    (postings:pst-remove pst 5)
    (check (postings:get-list pst) '())))

(defun sorted-p (lst &optional (prev 0))
  (cond ((null lst) t)
	((< prev (first lst)) (sorted-p (rest lst) (first lst)))
	(t nil)) )
