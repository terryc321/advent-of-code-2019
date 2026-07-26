
(defpackage :fun
  (:use :cl))
(in-package :fun)

;; (declaim (optimize (debug 3)))

(defparameter *hash* (make-hash-table))

;; (ql:quickload :uiop)
;;(ql:quickload :str)
;;(str:split "=>" "foo=>bar=>baz")
(defun input ()  (mapcar #'splitter (uiop:read-file-lines "../input.txt")))
(defun example1 ()  (mapcar #'splitter (uiop:read-file-lines "../example1.txt")))
(defun example2 ()  (mapcar #'splitter (uiop:read-file-lines "../example2.txt")))
(defun example3 ()  (mapcar #'splitter (uiop:read-file-lines "../example3.txt")))
(defun example4 ()  (mapcar #'splitter (uiop:read-file-lines "../example4.txt")))
(defun example5 ()  (mapcar #'splitter (uiop:read-file-lines "../example5.txt")))



(defun splitter (s)
  (let* ((sides (str:split "=>" s))
	 (left (apply #'append (mapcar #'splitter2 (mapcar #'str:trim (str:split "," (first sides))))))
	 (right (splitter2 (str:trim (second sides)))))
    ;;(format t "sides =~a~%left = ~a~%right = ~a~%~%" sides left right)
    (list left right)))

;; to avoid symbol conflicts we prefix all interned symbols with P
;; fuel becomes PFUEL 
(defun splitter2 (s)
  (let* ((sides (str:split " " s))
	 (left (parse-integer (first sides)))
	 (right (intern (concatenate 'string "P" (second sides)))))
    ;;(format t "splitter2:sides =~a~%left = ~a~%right = ~a~%~%" sides left right)
    (list left right)))

(defun update-hash (recipe)
  (setq *hash* (make-hash-table))
  (mapcar (lambda (x)
	    (let ((key (second (second x)))
		  (val x))
	      (setf (gethash key *hash*) val)))
	  recipe)
  nil)


(defun simplified? (sym)
  (let* ((equation (gethash sym *hash*))
	 (left (first equation)))
    (and (= (length left) 2)
	 (eq (second left) 'pore))))


#|
10 ORE => 10 A
1 ORE => 1 B
7 A 1 B => 1 C
7 A 1 C => 1 D
7 A 1 D => 1 E
7 A 1 E => 1 FUEL

7A 1E

7A can be got 10 ORE => 10 A remain 3 spare A cost 10 ORE
1E = 7A 1D
1D = 7A 1C
1C = 7A 1B
1B = 1ORE


expand
compress - total up number required 

FUEL
= 7A 1E
= 7A 7A 1D
= 7A 7A 7A 1C
= 7A 7A 7A 7A 1B
= 28A 1B

30ORE= 30A
1ORE = 1B
31 ORE = 30A + 1B
sufficient for 28A + 1B

excess of 2 A 

|#

;; observation (1)
;; notice each line involves ORE is only ever associated with ONE symbol
;; observation (2)
;; if a symbol is associated with ONE ORE then it is in some sense most simplified
;; hypothesis (3)
;; once the fuel equation has been reduced to simplified symbols , we can compute the ore
;; required ?

(defun solve(recipe)
  (update-hash recipe)
  ;; (solve-for 1 'fuel)
  )

(defun simplify-term(quant sym)
  (cond ;; ore prefixed p
    ((eq sym 'pore) (list quant sym))
    (t (let* ((equation (gethash sym *hash*))
	      (quant2 (first (second equation)))
	      (left (first equation)))
	 (format t "checking lookup ~a ..." sym)
	 (assert equation)
	 (assert quant2)
	 (assert left)
	 (format t "ok~%")
	 (cond
	   ((and (= (length left) 2) ;; avoid commit to pore immediately
		 (eq (second left) 'pore))
	    (list quant sym))
	   ((= quant quant2) ;; cost ore completely expended no spare capacity
			     (format t "simplify-exact expenditure~%")
			     left)
	   ((> quant quant2) ;; insufficient need to multiply each component by a factor
			     (format t "simplify-insufficient expenditure~%")
			     (let ((factor (multiple-value-bind (whole part)
					       (floor quant quant2)
					     (cond
					       ((zerop part) whole)
					       (t (+ 1 whole))))))
			       (format t "factor computed ~a~%" factor)
			       (let ((res (mapcar #'(lambda (expr)
						      (cond
							((integerp expr) (* factor expr))
							(t expr)))
						  left)))
				 res)))
	   ((< quant quant2)
	    (format t "simplify-excess expenditure~%")
	    left))))))

	   
;;; by-pairs


	   
	   
(defun simplify(expr)
  (cond
    ((null expr) expr)
    (t (let ((n1 (first expr))
	     (sym (second expr))
	     (rest (cdr (cdr expr))))
	 (append (simplify-term n1 sym)
		 (simplify rest))))))


(defun normalize (expr)
  (let ((norm nil))
    (catch 'done
      (loop while t do
	(setq norm (compress (simplify expr)))
	(when (equalp norm expr)
	  (throw 'done norm))
	(setq expr norm)))))


(defun get-symbols(expr)
  (labels ((helper (expr acc)
		     (cond
		       ((null expr) acc)
		       (t (let ((sym (second expr))
				(rest (cdr (cdr expr))))
			    (cond
			      ((member sym acc) (helper rest acc))
			      (t (helper rest (cons sym acc)))))))))
	  (helper expr '())))

;; (assoc 'a '((a . 1)(b . 2)))
;; (assoc 'b '((a . 1)(b . 2)))
;; (assoc 'c '((a . 1)(b . 2)))
;; (acons 1 2 '())

(defun compress (expr)
  (let ((hash (make-hash-table)))
    (labels ((helper (expr)
	       (cond
		 ((null expr) nil)
		 (t (let ((n (first expr))
			  (sym (second expr))
			  (rest (cdr (cdr expr))))
		      (let ((as (gethash sym hash nil)))
			(cond
			  ((null as)
			   (setf (gethash sym hash) n)
			   (helper rest))
			  (t (let ((n2 as))
			       (setf (gethash sym hash) (+ n n2))
			       (helper rest))))))))))
      ;; build hash table 
      (helper expr)
      (let ((result '()))
	(maphash #'(lambda (key value)
		     (setq result (cons (cons key value) result)))
		 hash)
	(sort result (lambda (x y)
		       (string< (format nil "~a" x)
				(format nil "~a" y)))
	      :key #'car)
	(apply #'append
	       (mapcar (lambda (v)
			 (destructuring-bind (s . i) v (list i s)))
		       result))))))




;; compress an expression 7A 7A 7A 7A 1B = 28A 1B 
;; (defun compress (expr)
;;   (let ((asc '()))
;;     (labels ((helper (expr)
;; 	       (cond
;; 		 ((null expr) nil)
;; 		 (t (let ((n (first expr))
;; 			  (sym (second expr))
;; 			  (rest (cdr (cdr expr))))
;; 		      (let ((as (assoc sym asc)))
;; 			(cond
;; 			  ((null as)
;; 			   (setq asc (cons (cons sym n) asc))
;; 			   (helper rest))
;; 			  (t (let ((n2 (second as)))
;; 			       (setq asc (cons (cons sym (+ n n2)) asc))
;; 			       (helper rest))))))))))
;;       ;; build association list 
;;       (helper expr)
;;       ;; get all the symbols
;;       (let ((syms (get-symbols asc)))
;; 	(apply #'append
;; 	       (mapcar (lambda (s)
;; 			 (destructuring-bind (sy . n) (assoc s asc)
;; 			   (list n sy)))
;; 		       syms ))))))
		    
  
(defun run (xs)
  (solve xs)
  (normalize '(1 pfuel))
  )

(defun cost (quant sym)
  (assert (simplified? sym))
  (let ((out (gethash sym *hash*)))
    (assert out)
    (destructuring-bind ((quant2 pore) (quant3 s)) out 
      (assert (eq s sym))
      (assert (eq pore 'pore))
      (assert (integerp quant2))
      (assert (integerp quant3))
      (cond
	((= quant quant3)
	 ;;easy 1 to 1 translation 
	 quant2) 
	((< quant quant3)
	 ;; want 1 A but ((10 PORE) <= (3 A)) can only have 3 A , so pay extra discard 2A
	 quant2)	
	((> quant quant3)
	 ;; want 28 A but have ((10 PORE) <= (10 A))
	 (let ((factor (multiple-value-bind (whole part)
			   (floor quant quant3)
			 (cond
			   ((zerop part) whole)
			   (t (+ 1 whole))))))
	   (format t "factor computed ~a~%" factor)
	   (* factor quant2)))))))


(defun total-cost (expr)
  (let ((tot 0))
    (labels ((helper (expr)
	       (cond
		 ((null expr) nil)
		 (t (let ((quant (first expr))
			  (sym (second expr))
			  (rest (cdr (cdr expr))))
		      (setq tot (+ tot (cost quant sym)))
		      (helper rest))))))
      (helper expr)
      tot)))

      

;; (total-cost (run (example1))) => 31 : ok
;; (total-cost (run (example2))) => 165 : ok
;; (total-cost (run (example3))) => 4880 : bad 13312
;; (total-cost (run (example4))) => 77614 : bad expect 180697
;; (total-cost (run (example5))) => 2092311 : bad expect 2210736



  
  ;; (simplify '(1 pfuel))
  ;; (simplify '(7 pa 1 pe))
  ;; (compress (simplify *)))


;; (defun solve-for(quantity sym)
;;   (cond
;;     ((simplified? sym) (list quantity sym))
;;     (t     
;;      (let ((equation (gethash sym *hash*)))
;;        (assert equation)
;;        (let* ((quantity2 (first (second equation)))
;; 	      (factor (the-factor quantity quantity2))
;; 	      (sum 0))
;; 	 (format t "the-factor ~a vs ~a ~%" quantity quantity2)
;; 	 (when (>= quantity2 quantity)
;; 	   (setq factor 1))
;; 	 (dolist (pair (first equation))	   
;; 	   (destructuring-bind (quant symb) pair
;; 	     (let ((extra (* factor (solve-for quant symb))))
;; 	       ;;(format t "extra ~a~%" extra)
;; 	       ;;(incf sum extra)
;; 	       (setq sum (+ sum extra)
;; 	       ))))
;; 	 (format t "solving for ~a ~a => with ~a => ~a ~%" quantity sym equation sum)
;; 	 sum)))))





;; (defun solve(recipe)
;;   (update-hash recipe)
;;   ;;(solve-for 1 'fuel)
;;   (let ((equation (gethash 'fuel *hash*)))
;;     (format t "fuel equation ~a ~%" equation)
;;     (mapcar (lambda (pair)
;; 	      (format t "solve-pair ~a~%" pair)
;; 	     (destructuring-bind (quant symb) pair
;; 	       (solve-for quant symb)))
;; 	   (first equation))))


;; (defun solve-for(quantity sym)
;;   (cond
;;     ((eq sym 'ore) quantity)
;;     (t     
;;      (let ((equation (gethash sym *hash*)))
;;        (assert equation)
;;        (let* ((quantity2 (first (second equation)))
;; 	      (factor (the-factor quantity quantity2))
;; 	      (sum 0))
;; 	 (format t "the-factor ~a vs ~a ~%" quantity quantity2)
;; 	 (when (>= quantity2 quantity)
;; 	   (setq factor 1))
;; 	 (dolist (pair (first equation))	   
;; 	   (destructuring-bind (quant symb) pair
;; 	     (let ((extra (* factor (solve-for quant symb))))
;; 	       ;;(format t "extra ~a~%" extra)
;; 	       ;;(incf sum extra)
;; 	       (setq sum (+ sum extra)
;; 	       ))))
;; 	 (format t "solving for ~a ~a => with ~a => ~a ~%" quantity sym equation sum)
;; 	 sum)))))
      
;; ;; (solve-for 12 fkmqd) => (((1 RVKX)) (1 FKMQD)) ... clearly multiplier factor = 12
;; ;; suppose was 
;; ;; (solve-for 13 fkmqd) => (((1 RVKX)) (3 FKMQD)) ... what multiplier factor ?
;; (defun the-factor (n1 n2)
;;   (multiple-value-bind (whole part) (floor n1 n2)
;;     (cond
;;       ((zerop part) whole)
;;       (t (+ whole 1)))))


;;(eval (solve (example1))) ;; => 31
;;(eval (solve (example2))) ;; => 165 
;;(solve (input))
;;
;;
  


;; split each line by => marker
;; split left hand side by , comma marker
;; 

#|

example 1 

10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL

example1
is it just a case of rewriting 1 FUEL -> 7A + 1E
7A + 7A + 1D
7A + 7A + 7A + 1C
7A + 7A + 7A + 7A + 1B
28A + 1B

1 ORE = 1B
28A + 1_ore
30_ore + 1_ore
31_ore

subsitute constant * symbol , by symbol we mean a sequence of letters

start at fuel equation and rewrite until just ore's 

|#



#|
example 2

9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL

1 FUEL
========
2 AB
3 BC
4 CA
========
2 AB = 6 A  + 8 B
3 BC = 15 B + 21 C
4 CA = 16 C + 4 A
====
10 A 
23 B
37 C
====
45 ORE = 10 A
64 ORE = 24 B
56 ORE = 40 C
====
165 ORE
====

|#

;; (defun mby (eq n)
;;   (assert (integerp n))
;;   (mapcar (lambda (x)
;; 	    (cond
;; 	      ((integerp x) (* x n))
;; 	      (t x)))
;; 	  eq))

;; (mby '(2 ab 


