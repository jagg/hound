
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
  #'test-add)

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
