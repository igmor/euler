;;;(euler-problem-1 999)
(defun euler-problem-1 (n)
  (if (> n 0)
      (if (or (eq (mod n 3) 0) (eq (mod n 5) 0)) 
	  (+ n (euler-problem-1 (- n 1))) 
	  (euler-problem-1 (- n 1)) )
      0))

;;;(euler-problem-2 1 2)
(defun euler-problem-2 (n1 n2)
  (if (< n2 4000000)
      (if (eq (mod n2 2) 0)
	  (+ n2 (euler-problem-2 n2 (+ n1 n2)))
	  (euler-problem-2 n2 (+ n1 n2)))
      0))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

;;;(euler-problem-3 600851475143)
(defun euler-problem-3 (n)
  (do ((i 2 (+ i 1))
       (p n (do ((j p (/ j i))) 
	      ((or (/= (mod j i) 0) (eq j i)) j))))
       ((> i (isqrt n)) p)))


(defun euler-problem-4 ()
  (do ((i 999 (decf i))
       (maxp 0))
      ((< i 100) maxp)
    (setf maxp (do ((j 999 (decf j))
	 (s "1234" (format nil "~a" (* i j))))
	((or (< j 100) (< (* i j) maxp) ) maxp)
		 (if (string-equal s (reverse s)) (return (* i j)))))))

(defun euler-problem-5()
  ; trivial 1*2*3*5*7*3*11*2*13*2*2*17*19
)	

(defun euler-problem-6()
  (do ((i 1 (+ i 1))
       (ssq 0 (+ (* i i) ssq))
       (sqs 0 (+ sqs i)))
      ((eq i 101) (- (* sqs sqs) ssq))))

;;; n =10001
(defun euler-problem-7(n)
  (do ((i 2 (+ i 1))
       (p 0 (if (primep i) (+ p 1) p)))
      ((eq p n) (- i 1))))

(defun divisors(n)
  (do ((i 2 (+ 1 i))
       (l nil (do ((l l (push i l)))
		  ((/= (mod n i) 0) l)
		(setf n (/ n i)))))
      ((eq n 1) l)))

;accepts list of prime divisors
;retusn all proper divisors
(defun divisors-all(n)
  (do ((i 2 (+ 1 i))
       (l nil (do ((l l (progn (push i l) (push n l))))
		  ((/= (mod n i) 0) l)
		(setf n (/ n i)))))
      ((eq n 1) l)))

(defun n-divisors(l)
  (do ((i 0 (+ 1 i)))
      ((or (/= (nth i l) (first l)) (not l))
       (if (< i (list-length l)) 
	   (append (list i) (n-divisors (nthcdr i l))) 
	   (list i)))))
   ; (format t "n-div ~d ~a " i l)))

(defun nlist-divisors(l)
  (if (not l) '()
      (do ((i 0 (+ 1 i)))
	  ((/= (nth i l) (nth (1+ i) l)) 
	   (nconc (list (subseq l 0 (1+ i))) 
		  (nlist-divisors (subseq l (1+ i))))))))

(defun proper-divisors(l)
  (if (not l) '()
      (do* ((s (car l) (rest s))
	    (pr (car s) (* pr (if s (car s) 1)))
	    (res '()))
	   ((not s) res)
	(progn (format t "~d ~a~%" pr s)
	     (append (nconc (list pr) (proper-divisors (rest l))) res)))))
    

(defun list-product(l)
  (if (first l) (* (+ (first l) 1) (list-product (rest l))) 1))

(defun list-sum(l)
  (if (first l) (+ (first l) (list-sum (rest l))) 0))

(defun euler-problem-12(n)
  (do ((i 1 (+ i 1)))
      ((> (list-product 
	    (n-divisors 
	     (rest 
	      (sort (append (divisors i) (divisors (+ i 1))) #'<)
	      ))) 
	   n) 
       (/ (* i (+ i 1)) 2))))
 
(defun seq-path(n)
  (if (eq n 1) (list 1)
      (if (eq (mod n 2) 0) 
	  (append (list n) (seq-path (/ n 2)))
	  (append (list n) (seq-path (+ (* n 3) 1))))))

;; n=1000000
(defun euler-problem-13(n)
  (do ((i n (- i 1))
       (pnum n)
       (ppath 0 (list-length (seq-path i)))
       (pmax 0 (if (> ppath pmax) (progn (setf pnum (+ i 1)) ppath) pmax)))
      ((<= i 0) pnum)))  

(defun sum-digits(n)
  (if (plusp n) 
      (+ (mod n 10) (sum-digits (floor (/ n 10)))) 
      0))

;;n=1000
(defun  euler-problem-16(n)
  (sum-digits (expt 2 n)))

(defstruct graph-node
  conns
  label
  value)

(defun get-node(label g)
  	 (first (remove-if-not 
	    #'(lambda(x) (string= (graph-node-label x) label)) g)))

(defun add-edge (g vlabel nlabels)
  (push  
	 (remove-if-not 
	  #'(lambda(x) 
	      (find-if  #'(lambda(y) (string= y (graph-node-label x))) nlabels)) g)
	 (graph-node-conns 
	  (get-node vlabel g))))
 
(defun add-edges (g edges)
  (loop for edge in edges do
       (add-edge g (first edge) (second edge))))

(defun gen-graph(labels)
  (mapcar #'(lambda (x) (make-graph-node :conns nil :label (first x) :value (second x))) labels))

(defun create-graph(vertexes edges)
  (let ((g (gen-graph vertexes)))
       (progn (add-edges g edges) g)))

(defvar *np*)
(setf *np* 0)

(defun create-graph-2x2()
    (create-graph 
     '("00" "01" "02" "10" "11" "12" "20" "21" "22") ;vertexes
     '(
       ("00" ("01" "10")) ("01" ("02" "11")) ("02" ("12")) ;;edges
       ("10" ("20" "11")) ("11" ("12" "21")) ("12" ("22")) 
       ("20" ("21")) ("21" ("22")))))

(defun make-graph-edge(i &rest connectedto)
  (list i connectedto))

(defun concat(i j)
  (concatenate 'string 
	       (if (< i 10) "0") (write-to-string i)
	       (if (< j 10) "0") (write-to-string j)))

(defun create-graph-mxn(m n)
  (let (v e)
    (dotimes (i m) 
       (dotimes (j n)
	 (progn (push (concat i j) v)
		(if (and (< i (- m 1)) (< j (- n 1)))
		    (push
		     (make-graph-edge (concat i j) (concat (+ 1 i) j) (concat i (+ 1 j))) e)
		    (if (and (= i (- m 1)) (< j (- n 1))) 
			(push (make-graph-edge (concat i j) (concat i (+ 1 j))) e)
			(if (and (= j (- n 1)) (< i (- m 1))) 
			    (push (make-graph-edge (concat i j) (concat (+ 1 i) j)) e)))))))
		
(create-graph (reverse v) (reverse e))))


(defun graph-traverse(g slabel elabel)
  (if (string= (graph-node-label g) elabel)
      (incf *np*)
      (loop for neigbor in (graph-node-conns g) do
	 ; (progn (format t "~a~%" neigbor ) 
	   (graph-traverse neigbor slabel elabel))))

(defun multiply-matrixes(m1 m2)
  (let ((result (make-array (array-dimensions m1))))
	 (dotimes (i (first (array-dimensions m1))) 
	  (dotimes (j (second (array-dimensions m2)))
	    (setf (aref result i j)
		   (let ((s 0)) 
		     (dotimes (k (second (array-dimensions m1)))
		       (setf s (+ s (* (aref m1 i k) (aref m2 k j))))) s)))) result))

(defun matrix-transpose(m)
  (let ((n (first (array-dimensions m)))
	temp)
  (dotimes (i n)
    (loop for j from i to (1- n) do
      (progn 
	(setf temp (aref m i j))
	(setf (aref m i j) (aref m j i))
	(setf (aref m j i) temp)))) m))

(defun create-graph-adj-matrix(m n)
 (let ((result (make-array (list (* (1+ m) (1+ n)) (* (1+ m) (1+ n))))))
    (dotimes (i (1+ m)) 
       (dotimes (j (1+ n))
	 (if (and (< i m) (< j n))
	     (let ((i1 (+ (* i (1+ n)) j)) 
		   (i2 (+ 1 (* i (1+ n)) j)) 
		   (i3 (+ (* (1+ i) (1+ n)) j)) )
		   (progn (setf (aref result i1 i2) 1) 
			  (setf (aref result i1 i3) 1)
			  ;(format t "[~d ~d ~d]" i1 i2 i3)
			  ))

	     (if (and (= i m) (< j n)) 
		 (let ((i1 (+ (* i (1+ n)) j)) 
		       (i2 (+ 1 (* i (1+ n)) j)) )
		   (setf (aref result i1 i2) 1)) 
		 (if (and (= j n) (< i m)) 
		     (let ((i1 (+ (* i (1+ n)) j)) 
			   (i3 (+ (* (1+ i) (1+ n)) j)) )
		       (setf (aref result i1 i3) 1) )))))) result))

(defun matrix-identity(n)
  (let ((m (make-array (list n n))))
    (dotimes (i n)
      (setf (aref m i i) 1)) m))

(defun matrix-exp(m n)
    (let ((result m))
      (dotimes (i (1- n))
	(setf result (multiply-matrixes result m))) result))

(defun euler-problem15 ()
  (matrix-exp (create-graph-adj-matrix 20 20) 40))

(defun factorial(n)
  (if (zerop n) 1
      (* n (factorial (1- n)))))

(defun euler-problem20 ()
  (sum-digits (factorial 100)))


(defparameter *number-dict* (make-hash-table))

(progn
  (setf (gethash 1 *number-dict*) "one")
  (setf (gethash 2 *number-dict*) "two")
  (setf (gethash 3 *number-dict*) "three")
  (setf (gethash 4 *number-dict*) "four")
  (setf (gethash 5 *number-dict*) "five")
  (setf (gethash 6 *number-dict*) "six")
  (setf (gethash  7 *number-dict*) "seven")
  (setf (gethash  8 *number-dict*) "eight")
  (setf (gethash  9 *number-dict*) "nine")
  (setf (gethash  10 *number-dict*) "ten")
  (setf (gethash  11 *number-dict*) "eleven")
  (setf (gethash  12 *number-dict*) "twelve")
  (setf (gethash  13 *number-dict*) "thirteen")
  (setf (gethash  14 *number-dict*) "fourteen")
  (setf (gethash  15 *number-dict*) "fifteen")
  (setf (gethash  16 *number-dict*) "sixteen")
  (setf (gethash  17 *number-dict*) "seventeen")
  (setf (gethash  18 *number-dict*) "eighteen")
  (setf (gethash  19 *number-dict*) "nineteen")
  (setf (gethash  20 *number-dict*) "twenty")
  (setf (gethash  30 *number-dict*) "thirty")
  (setf (gethash  40 *number-dict*) "forty")
  (setf (gethash  50 *number-dict*) "fifty")
  (setf (gethash  60 *number-dict*) "sixty")
  (setf (gethash  70 *number-dict*) "seventy")
  (setf (gethash  80 *number-dict*) "eighty")
  (setf (gethash  90 *number-dict*) "ninety"))

(defun euler-problem17()
  (let ((s 0))
    (loop for i from 1 to 999 do
	 (let ((n i))
	   (progn (if (>= (/ n 100.) 1)
		      (progn ;(format t "~d hundred" (gethash (floor (/ n 100.)) *number-dict*))
			(setf s (+ s (length (gethash (floor (/ n 100.)) *number-dict*)) (length "hundred")))
			(setf n (- n (* (floor (/ n 100.)) 100)))
			(if (> n 0) (setf s (+ s 3))))) ;; 'and'
		  (if (>= (/ n 10.) 2)
		      (progn ;(format t "~d " (gethash (* 10 (floor (/ n 10.))) *number-dict*))
			(setf s (+ s (length (gethash (* 10 (floor (/ n 10.))) *number-dict*))))     
			(setf n (- n (* (floor (/ n 10.)) 10)))))
		  (if (>= n 1)
		      (setf s (+ s (length (gethash n *number-dict*)))))))) (+ s (length "onethousand"))))
		      ;(format t " ~d~% " (gethash n *number-dict*))
		      ;(format t "~%"))))))

(defvar *triangle*)

(setf *triangle*
  '((75)
   (95 64)
   (17 47 82)
   (18 35 87 10)
   (20 04 82 47 65)
   (19 01 23 75 03 34)
   (88 02 77 73 07 63 67)
   (99 65 04 28 06 16 70 92)
   (41 41 26 56 83 40 80 70 33)
   (41 48 72 33 47 32 37 16 94 29)
   (53 71 44 65 25 43 91 52 97 51 14)
   (70 11 33 28 77 73 17 78 39 68 17 57)
   (91 71 52 38 17 14 91 43 58 50 27 29 48)
   (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
   (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

(defun build-triangle-graph()
  (let (e v)
    (dotimes (i (length *triangle*))
      (dotimes (j (length (elt *triangle* i)))
	;(format t "~d~%" ( elt (elt *triangle* i) j)))))
	(progn 
	  ;(format t "~d ~d~%" i j)
	  (push (list (concat i j) ( elt (elt *triangle* i) j)) v)
	  (if (and (> i 0) (> j 0) (< j (- (length (elt *triangle* i)) 1)))
	      (progn 
		(push
		 (make-graph-edge (concat (1- i) (1- j)) (concat i j)) e)
		(push
		 (make-graph-edge (concat (1- i) j) (concat i j)) e))
	      (if (and (> i 0) (= j 0) ) 
		  (push (make-graph-edge (concat (1- i) j) (concat i j)) e)
		  (if (and (> i 0) (= j (1- (length (elt *triangle* i))))) 
		      (push (make-graph-edge (concat (1- i) (1- j)) (concat i j)) e)))))))

    (create-graph (reverse v) (reverse e))
    ;(format t "~a ~a ~%" (reverse v) (reverse e))
))

(defun build-triangle-graph2()
  (let (e v)
    (dotimes (i (length *triangle*))
      (dotimes (j (length (elt *triangle* i)))
	;(format t "~d~%" ( elt (elt *triangle* i) j)))))
	(progn 
	  ;(format t "~d ~d~%" i j)
	  (push (list (concat i j) ( elt (elt *triangle* i) j)) v)
	  (if (< i (1- (length *triangle*)))
	      (progn 
		(push
		 (make-graph-edge (concat i j) (concat (1+ i) j) (concat (1+ i) (1+ j))) e))))))
    (create-graph (reverse v) (reverse e))
    ;(format t "~a ~a ~%" (reverse v) (reverse e))
))

(defvar *maxp* 0)
(setf *maxp* 0)

(defun traverse-graph-for-max(g s)
  (if (eq (graph-node-conns g) nil) 
      (if (> (+ s (graph-node-value g)) *maxp*)
	  (progn (format t "~a~%" (graph-node-value g))
		 (setf *maxp* (+ s (graph-node-value g)))))
      (dolist (item (first (graph-node-conns g)))
	;(format t "~a~%" item))))
	(progn (format t "~a~%" (graph-node-value g)) ( traverse-graph-for-max item (+ s (graph-node-value g)))))))
      
(defun euler-problem18()
  (let ((g (build-triangle-graph2)))
    (traverse-graph-for-max (get-node "0000" g) 0) *maxp*))

(defun euler-problem19()
  (let ((s 0) 
	(d 2) 
	(m '(31 28 31 30 31 30 31 31 30 31 30 31)))
    (loop for i from 1901 to 2000 do
	 (dolist (l m)
	   (progn 
	     ;(format t "~d  ~d ~d~%" i (mod d 7) s)
	     (if (eq (mod d 7) 0)
		 (incf s))
	     (if (and (eq l 28) (eq (mod i 4) 0))
		 (incf d)
	     (setf d (+ d l)))))) s))

;n=10000
(defun euler-problem21(n)
  (let ((s 0))
    (dotimes (i n)
      (let ((am (1+ (list-sum (divisors i)))))
	(if (eq (1+ (list-sum (divisors am))) i)
	    (incf s)))) s))

;n=1000
(defun euler-problem48(n)
  (let ((s 0))
    (dotimes (i n)
      (setf s (+ s (expt (1+ i) (1+ i))))) s))

;a=100, b=100
(defun euler-problem29(a b)
  (let (l)
    (loop for ai from 2 to a do
	 (loop for bi from 2 to b do
	      (push (expt ai bi) l))) (length (remove-duplicates l))))

(defun split-string(str delim)
  (let ((token nil) (l))
    (dotimes (i (length str))
	 (if (and token (string= (elt str i) delim))
	     (progn (push (reverse token) l) (setf token nil))
	     (push (elt str i) token))) l))

(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun word-score(word)
  (let ((score 0) 
	(alphabet "\"ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (dotimes (i (length word))
      (setf score 
	    (+ score 
	       (position (elt word i) alphabet)))) 
    score))

(defun euler-problem22()
  (let ((score 0) 
	(in (open "~/work/lisp/names.txt")))
    (when in
      (loop for line = (read-line in nil)
         while line do 
	   (let ((l (sort (split-sequence #\, line) #'string<)))
	     (dotimes (i (length l))
	       (setf score (+ score (* (1+ i) (word-score (elt l i))))))))
	   (close in)) score))

(defun euler-problem41()
  (let ((s 0))
    (dotimes (i 9999999999)
      (if (and (eq (length (remove-duplicates (write-to-string i)))
	      (length (write-to-string i))) (primep i))
	  (format t "~d~%" i))) s))

(defun num-digits(n)
  (if (> n 0) 
      (1+ 
       (num-digits (floor (/ n 10)))) 
      0))

;n=1000
(defun euler-problem25(n)
  (do* ((f1 1 f2)
       (f2 1 f)
       (f 2 (+ f1 f2))
       (tn 3 (+ 1 tn)))
      ((eq (num-digits f) n) tn)))
;    (format t "~d ~d ~d ~d ~%" f1 f2 f tn)))

(defun length-unique(l)
  (length (remove-duplicates l)))

(defun euler-problem47()
    (do* ((n1 2 n2)
	  (n2 3 n3)
	  (n3 4 n4)
	  (n4 5 (+ 1 n4)))
	 ((and (eq (length-unique (divisors n1)) 4)
	       (eq (length-unique (divisors n2)) 4)
	       (eq (length-unique (divisors n3)) 4)
	       (eq (length-unique (divisors n4)) 4)) n1)))
;    (format t "~d ~d ~d ~d ~%" f1 f2 f tn)))
    
;n=1001
(defun euler-problem28(n)
  (do* ((i 0 (+ i 2))
       (k 1 (+ k (* 4 i)))
	(s 0))
      ((> (+ 2 i) n) (+ s k))
    (dotimes (j 4)
      (setf s (+ s (+ k (* (+ i 2) j)))))))
;	     (format t "~d~%"  s)))))


(defun list-digits(n)
  (if (> n 0)
      (append (list (mod n 10)) (list-digits (floor (/ n 10))))
      '()))

(defun same-digitsp(n1 n2)
  (every #'= (sort (list-digits n2) #'<)
	 (sort (list-digits n1) #'<)))

(defun euler-problem49()
    (do ((n 1000 (+ n 1)))
	((> n 9999) nil)
      (loop for i from 1 to (floor (/ (- 10000 n) 2)) do
	   (if (and (same-digitsp n (+ n i)) 
		    (same-digitsp (+ n i) (+ n i i)) 
		    (primep n)
		    (primep (+ n i))
		    (primep (+ n i i)))
	       (format t "~d~d~d~%" n (+ n i) (+ n i i))))))

(defun cut-to-prime(l)
  (if (primep (reduce #'+ l))
      (reduce #'+ l)
      (cut-to-prime (cdr l))))

(defun euler-problem50(n)
  (do* ((i 2 (+ i 1))
	(s '(2) (if (primep i) (push i s) s))
	(result 0 (reduce #'+ s)))
       ((> result n) (cut-to-prime (reverse (rest s))))))

(defun euler-problem27()
    (let ((maxa -1000)
	  (maxb -1000)
	  (maxpnum 0))
      (loop for a from -999 to 999 do
	   (loop for b from -999 to 999 do
		(do ((n 0 (1+ n)))
		    ((not (primep (+ (* n n) (* a n) b)))
		     (if (> n maxpnum) (progn (setf maxpnum n)
					      (setf maxa a)
					      (setf maxb b))))))) (* maxa maxb)))
;n=999999
(defun euler-problem30(n)
  (if (eq n 1) 0
      (if (eq n 
	      (reduce #'+ (mapcar #'(lambda(x) (expt x 5)) 
				  (list-digits n))))
	     (+ n (euler-problem30 (- n 1)))
	     (euler-problem30 (- n 1)) )))
	     
	      
(defun digital-shift(s d)
  (let ((x s))
    (loop for i from 1 to (num-digits d) when (< x d) do
	 (setf x (* x 10))) 
    x))

(defun app-zero(n)
  (if (< n 100) 
      '() 
      (append '(0) (app-zero (/ n 10)))))

(defun divideby(n d)
  (do* ((s 
	 (digital-shift (if (> n d) (mod n d) n) d)
	 (digital-shift s d))
	(ld (app-zero s) (push (app-zero s) ld))) 
       ((eq (mod s d) 0) (push (/ s d) ld))
    (progn (format t "~a~%" ld) (push (floor (/ s d)) ld))))

(defun p (l)
  (if (null l) '(())
      (mapcan #'(lambda (x) 
		  (mapcar #'(lambda (y) (cons x y)) 
			  (p (remove x l :count 1)))) l)))

(defun euler-problem24()
  (elt (p '(0 1 2 3 4 5 6 7 8 9)) 999999))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list))  (list list))
        (t (loop for element in list
            append (mapcar (lambda (l) (progn (format t "~a~t" (cons element l)) (list (cons element l) l)))
                            ;(progn (format t "~a~%" (remove element list)) 
			   (all-permutations (remove element list)))))))

(defun n-proper-divs (N)
       (do ( (i 1 (1+ i))
       	     (l '() (if (= (mod N i) 0)
		    	 (append (list i) l) l)))
	    ((> i (/ N 2)) l) ))

(defun sump(L)
       (if (null L) 0
       	     	    (+ (car L) (sump (rest L)))))

(defun amicable(X)
       (do (  (i 1 (1+ i))
              (a1  0 (sump (n-proper-divs i))   )
	      (a2  0 (sump (n-proper-divs a1))  )
	      (s 0) 
	      )
           ((> a1 X) s)
	     (if  (= a2 i)
	     	 (setf s (+ s i a2))
		 s)))
;;;           (if (= (sump (n-proper-divs (sump (n-proper-divs i)))) i)
;;; 	       (format t "~d~%" i)
;;;	       nil)))
;;;	   (format t "~d ~d ~d~%" i a1 a2)))


 (defun abundantp(n)
 	(> (sump (n-proper-divs n)) n))

 (defun list-abundant()
 	(do ((i 1 (1+ i))
	    (l '() (if (abundantp i)
	       	       (push i l)
		       l)))
            ((> i 28123) l)))

 (defun n-sum-two-abundantp(n abl)
 	(dolist (e1 abl)
		(dolist (e2 abl)
			(if (= (+ e1 e2) n)
			    (return-from n-sum-two-abundantp t)
			    (if (and (> e1 n) (> e2 n))
			    	(return-from n-sum-two-abundantp nil))))))
			    
			
 (defun euler-problem23()
	(do ((l (reverse (list-abundant)))
	      (i 1 (1+ i))
	      (r '() (if (n-sum-two-abundantp i l)
	      	     r
	      	     (push i r))))
            ((> i 28123) (sump r))))


(defun find-cyclesp(d n)
       (if (< (length d) (num-digits n)) nil
              (search (subseq d 0 (num-digits n)) (subseq d (num-digits n)))))
       
(defun n-get-fractions(n)
       (do ((d '())
       	    (i 1 (if (< i n)
       	       	     (progn (if (< (* 10 i) n) (push 0 d)) (* i 10))
		     (progn (push (floor (/ i n)) d) (mod i n)))))
           ( (or (= i 0) (find-cyclesp d n)) (if (= i 0) d (subseq d (num-digits n))) )))
       	       
       	    

(defun euler-problem26()
       (do ((i 1 (1+ i))
       	    (d '() (n-get-fractions i))
       	    (max-cycle 0 (if (> (length d) max-cycle)
	    	       	     (progn (format t "number ~d~%" (- i 1)) (length d))
			     max-cycle)))
       ( (> i 1000) t)
         (format t "~d~%" max-cycle)))

(defun n-num-of-2pounds(p coins)
  (dolist (e coins)
    (if (> (- p e) 0)
	(+ (n-num-of-2pounds (- p e) coins))
	(if (= (- p e) 0) (return 1) (return 0)))))
	   