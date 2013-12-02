
(in-package #:postings)

(defgeneric pst-add (post-lst id &key comp-fn)
  (:documentation "Adds an element destructively to the sorted POSTING-LST"))

(defgeneric pst-remove (post-lst id &key key)
  (:documentation "Removes an element destructively from teh POSTING-LST."))

(defgeneric pst-union (post-lst1 post-lst2 &key comp-fn)
  (:documentation "Generations a new POSTING-LST with the elements of both lists"))

(defgeneric pst-intersection (post-lst1 post-lst2 &key comp-fn)
  (:documentation "Generates a new POSTING-LST only with the elements present in both lists"))

(defgeneric posting-p (elem)
  (:documentation "Checks if the element is a posting"))

(defmethod posting-p ((elem t))
  nil)

;; Naive implementation with a sorted linked list

(defclass linked-pst ()
  ((plst :initarg :plst :accessor plst)))

(defun make-linked-pst (&optional (lst nil))
  (make-instance 'linked-pst :plst lst))

(defun get-list (lplst)
  (plst lplst))

(defmethod posting-p ((elem linked-pst))
  t)


;; TODO This operations can be improved to take into account that
;; the list is sorted

(defmethod pst-add((post-lst linked-pst) id &key (comp-fn #'<))
  (with-accessors ((lst plst)) post-lst
    (setf lst (sort (cons id lst) comp-fn)))
  post-lst)

(defmethod pst-remove((post-lst linked-pst) id &key (key #'=))
  (with-accessors ((lst plst)) post-lst
    (setf lst (delete id lst :test key)))
  post-lst)

(defmethod pst-union ((post-lst1 linked-pst) (post-lst2 linked-pst) &key (comp-fn #'<))
  (lpst-join post-lst1 post-lst2 #'union :comp-fn comp-fn))

(defmethod pst-intersection ((post-lst1 linked-pst) (post-lst2 linked-pst) &key (comp-fn #'<))
  (lpst-join post-lst1 post-lst2 #'intersection :comp-fn comp-fn))

(defun lpst-join (post-lst1  post-lst2 operation &key (comp-fn #'<))
  (with-accessors ((lst plst)) post-lst1
    (with-accessors ((lst2 plst)) post-lst2
      (make-instance 'linked-pst :plst (sort (funcall operation lst lst2) comp-fn)))))

(defmethod print-object ((object linked-pst) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (plst) object
      (format stream "~a" plst))))
