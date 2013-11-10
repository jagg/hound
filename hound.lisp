;;;; hound.lisp

(in-package #:hound)

;; http://java.dzone.com/articles/algorithm-week-graphs-and?utm_source=twitterfeed&utm_medium=twitter&utm_campaign=Feed%3A+javalobby%2Ffrontpage+%28Javalobby+%2F+Java+Zone%29

;; FSTs: A naive implementation with cons cells

;; To make this multithreaded we just need to add
;; a lock to the state

;; Also, we can have a termID as the final flag
;; as a label for that term
(defstruct state 
  (value (error "Value must be provided"))
  (children '())
  (final nil))

(defstruct state-bis 
  (value (error "Value must be provided"))
  (transitions (make-hash-table)) ;; st1 -- a --> st2
  (final nil))

(defun empty-fst ()
  (make-state :value ""))

(defun fst-adder (fst)
  (let ((current fst)) 
    (lambda (c)
      (let ((next (make-state :value c)))
	(setf (state-children current) (adjoin next (state-children current)))
	(setf current next)))))

(defun add-term (fst term)
  (let ((last-el (car (last (map 'list (fst-adder fst) term)))))
    (setf (state-final last-el) t)
    fst))

