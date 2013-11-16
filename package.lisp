;;;; package.lisp

(defpackage #:hound
  (:use #:cl))

(defpackage #:trie
  (:use #:cl)
  (:export #:add-term
	   #:remove-term
	   #:make-trie
	   #:term-present-p
	   #:get-output
	   #:wild-card
	   #:wild-card-output
	   #:print-trie-file
	   #:print-trie))

(defpackage #:postings
  (:use #:cl)
  (:export #:pst-add
	   #:pst-remove
	   #:pst-union
	   #:pst-intersection
	   #:make-linked-pst))

