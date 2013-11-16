
(in-package #:trie)


(defstruct trie
  (init-st (make-state :id 0))
  (comp-fn #'char<)
  (key #'identity)
  (st-counter 1))

(defun next-id (trie)
  (let ((counter (trie-st-counter trie)))
    (setf (trie-st-counter trie) (+ 1 counter))
    counter))

;; A node in the trie
(defstruct state
  (id (error "Value must be provided"))
  (transitions (make-transitions)) ;; st1 -- a --> st2
  (output nil)
  (final nil))

(defun make-transitions (&optional (test #'eql))
  (make-hash-table :test test))


(defun add-transition (st1 v st2)
  "Adds a transition between st1 and st2 when 'v' is received"
  (setf (gethash v (state-transitions st1)) st2))

(defun transition (st v)
  "Returns the state reached from receiving v while in state 'st'"
  (when st (gethash v (state-transitions st))))


(defun add-term (trie term output)
  "Adds a term to the TRIE and associates the output in its last node"
  (labels ((adder (trie)
	     (let ((current (trie-init-st trie)))
	       (lambda (c)
		 (let* ((next (transition current c))
			(next (if (not next) 
				  (add-transition current c (make-state :id (next-id trie)))
				  next)))
		   (setf current next))))))
    (let ((last-node (car (last (map 'list (adder trie) term)))))
      (setf (state-final last-node) t)
      (setf (state-output last-node) output))))

(defun remove-term (trie term)
  "Removes the term from the TRIE, if present"
  (labels ((remove-term-rec (chars states remove-flag)
	     (if remove-flag
		 (let ((remove-st (<= (hash-table-count (state-transitions (car states))) 1)))
		   (remhash (car chars) (state-transitions (car states)))
		   (remove-term-rec (cdr chars) (cdr states) remove-st)))))
    (when (term-present-p trie term) 
      (let ((branch (get-term-branch trie term)))
	;; We add an extra char to pair states and characters in the recursion
	(remove-term-rec (cons #\. (coerce (reverse term) 'list)) branch t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export to DOT file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun state-repre (st)
  (if (state-output st)
      (format nil "\"~a/~a\"" (state-id st) (state-output st))
      (format nil "~a" (state-id st))))

(defun print-st (prev-id st-value st &optional (stream t))
  (when prev-id (format stream 
			   "~a -> ~a [label=\"~a\",weight=\"~a\"];~%" 
			   prev-id (state-repre st) st-value st-value))
  (maphash (lambda (k v) (print-st (state-repre st) k v stream)) (state-transitions st)))

(defun print-trie (trie &optional (stream t))
  (format stream "digraph {~%")
  (print-st nil "//." (trie-init-st trie) stream)
  (format stream "}~%"))

(defun print-trie-file (trie path)
  "Generates a dot file representing the trie in the specified path"
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (print-trie trie stream)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query the trie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-term-branch (trie term)
  (reduce (lambda (st-lst c)
	    (let ((next-st (transition (car st-lst) c)))
	      (if next-st (cons next-st st-lst)
		  st-lst))) 
	  term :initial-value (list (trie-init-st trie))))

(defun last-state (trie prefix)
  (reduce #'transition prefix :initial-value (trie-init-st trie)))


(defun term-present-p (trie term)
  "Predicate that checks if a term is included in the trie"
  (let ((last-st (last-state trie term))) 
    (if last-st (state-final last-st)
	nil)))

(defun get-output (trie term)
  "Retrieves the output for a given term, if present in the trie"
  (let ((last-st (last-state trie term))) 
    (if last-st (state-output last-st)
	nil)))

(defun retrieve-terms (state prefix)
  (let ((lst '()))
    (labels ((rt-rec (st w)
	       (when (state-final st) (setf lst (nconc lst (list w))))
	       (maphash (lambda (k v) (rt-rec v (concatenate 'string w (list k)))) 
			(state-transitions st))))
      (rt-rec state prefix)
      lst)))

(defun wild-card (trie prefix)
  "Returns all the terms in the trie the start with 'prefix'"
  (retrieve-terms (last-state trie prefix) prefix))

(defun wild-card-output (trie prefix)
  "Returns a list of the outputs associated to all the terms that start with 'prefix'"
  (mapcar (lambda (term) (get-output trie term)) (wild-card trie prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparison (only used for testing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *long-text* "“Evolution” attempts to simulate a very simple world where animals roam in directions dictated by their genes, eating food and reproducing asexually. Reproductions cause random changes to genes and hence the movement of animals. The center of the game world has more food resources than the rest, which means we can expect different types of animal to evolve in the middle than the rest of the world. It’s apparently based on article by A.K. Dewdney: “Simulated evolution: wherein bugs learn to hunt bacteria” (searching for this will find you some interesting resources). Full source code for the Common Lisp version is in the book as well as on the Land of Lisp website, the Clojure version is available on github.")

(defun gen-trie (text)
  (let ((terms (split-sequence:split-sequence #\Space text))
	(trie (make-trie)))
    (map 'list (lambda (term) (add-term trie term t)) terms)
    trie))

(defun gen-set (text)
  (let ((terms (split-sequence:split-sequence #\Space text))
	(table (make-hash-table)))
    (map 'list (lambda (term) (setf (gethash term table) t)) terms)
    table))
