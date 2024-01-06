

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...

;; regular expression
(use-modules (ice-9 regex)) 

(use-modules (ice-9 match)) 

;; --------------------- macros --------------------------
;; can we not do this using hygienic macros ...

(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			    ,@body
			    (,fn (cdr ,xs))))))))
       (,fn ,ls))))

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

;;(chdir "day3")
;; get current working directory
;;  (getcwd)


(define *debug* #f)

(define input #f)

(define (get-input filename)
  (let ((port (open-input-file filename)))
    (set! input (read port))
    (close-input-port port)))

;; for example
(define input #f)

(get-input "input")
;;(get-input "input2")


#|

trying to write this in neovim no plugins yet , just getting to grips with trying write some guile scheme code

(format #t "input ~a ~%" input)
(format #t  "input ~a ~%" (car input))
(format #t  "input ~a ~%" (caar input))
|#


#|

two lines 
line1 
line2

each line composed symbols 
L-XXX
U-XXX
D-XXX
R-XXX

left down up right

|#

;; turns symbol like R125 to (right 125)
(define (sym2op sym)
  (let ((str (symbol->string sym)))
    (let ((dir (string-ref str 0)))
      (cond
	((or (char=? dir #\R) (char=? dir #\r)) (set! dir 'right))
       	((or (char=? dir #\D) (char=? dir #\d)) (set! dir 'down))
	((or (char=? dir #\U) (char=? dir #\u)) (set! dir 'up))
	((or (char=? dir #\L) (char=? dir #\l)) (set! dir 'left))
	(#t (error 'dir (list dir))))
      (let ((dist (string->number (substring str 1 (string-length str)))))
	(list dir dist)))))


;;(assert (equal? '(right 123) (sym2op 'R123)))


#|
process a line of instructions and return a hash table of all coordinates we put our wire across
|#
(define (process-line xs)
  (let ((hash (make-hash-table))
	(x 0)
	(y 0))
    (hash-set! hash (list 0 0) #t)
    (letrec ((left (lambda (n) 
		     (cond
		      ((< n 1) 'done)
		      (#t (set! x (- x 1))
			  (hash-set! hash (list x y) #t)
			  (left (- n 1))))))
	     (right (lambda (n) 
		     (cond
		      ((< n 1) 'done)
		      (#t (set! x (+ x 1))
			  (hash-set! hash (list x y) #t)
			  (right (- n 1))))))
	     (up (lambda (n) 
		     (cond
		      ((< n 1) 'done)
		      (#t (set! y (+ y 1))
			  (hash-set! hash (list x y) #t)
			  (up (- n 1))))))
	     (down (lambda (n) 
		     (cond
		      ((< n 1) 'done)
		      (#t (set! y (- y 1))
			  (hash-set! hash (list x y) #t)
			  (down (- n 1))))))
	     (rec (lambda (xs) 
		    (cond
		     ((null? xs) 'done)
		     (#t (let ((instruction (sym2op (car xs))))
			   (let ((op (car instruction))
				 (dist (cadr instruction)))
			     (cond
			      ((eq? op 'left) (left dist))
			      ((eq? op 'right) (right dist))
			      ((eq? op 'up) (up dist))
			      ((eq? op 'down) (down dist))
			      (#t (error 'instruction (list op dist))))
			     ;; next instruction
			     (rec (cdr xs)))))))))
      (rec xs)
      hash)))

;;(process-line '(R8 U5 L5 D3))


#|
visualiser for a single wire line
(viz '(R8 U5 L5 D3))
 
|#
(define (viz line)
  (let* ((hash (process-line line))
	 (hash-list (hash-map->list cons hash)))
    (let ((min-x #f)
	  (max-x #f)
	  (min-y #f)
	  (max-y #f))
      ;; pass 1
      (dolist (entry hash-list) 
	      (match entry
		(((x y) . r) 
		 ;;(format #t "x = ~a : y = ~a ~%" x y)
		 (when (not min-x) (set! min-x x))
		 (when (not max-x) (set! max-x x))
		 (when (not min-y) (set! min-y y))
		 (when (not max-y) (set! max-y y))
		 (when (< x min-x)
		   (set! min-x x))
		 (when (> x max-x)
		   (set! max-x x))
		 (when (< y min-y)
		   (set! min-y y))
		 (when (> y max-y)
		   (set! max-y y)))))
      (viz2 hash min-x max-x min-y max-y)
      )))

#|
(viz '(R1))

(viz '())

(viz '(R8 D8))
(viz '(R8 U8))

(viz '(R8 U8 L8))
(viz '(R8 U8 L8 U8))

(viz '(R8 U5 L5 D3))
|#


#|

visualiser .........

vis2 displays a 2 dimensional grid 
starts at high Y
..
..
 to low Y
low X ........ high X
|#
(define (viz2 h x1 x2 y1 y2)
  (letrec ((foo (lambda (x y)
		  (cond
		   ((> x x2) 
		    (format #t "~%")
		    (foo x1 (- y 1)))
		   ((< y y1) 'done)
		   (#t
		    (let ((wire (hash-ref h (list x y))))
		      (cond
		       (wire (format #t "*"))
		       (#t (format #t "_")))
		      (foo (+ x 1) y)))))))
    ;; low-x high-y
    (foo x1 y2)))


;;(dolist (x '(1 2 3)) (format #t "x = ~a ~%" x))


#|
where do they cross ?
if have all coordinates from line 1 and check if on line 2 then find where they all cross
|#
(define (process two-lines)
  (assert (= 2 (length two-lines)))  
  (let* ((line1 (first two-lines))
	 (line2 (second two-lines))
	 (hash1 (process-line line1))
	 (hash2 (process-line line2)))
    (let ((crosses '()))
      (let ((hash-line (hash-map->list cons hash1)))
	(dolist (entry hash-line) 
		(match entry
		  (((x y) . r)
		   (when (hash-ref hash2 (list x y))
		     (set! crosses (cons (list x y) crosses))))))
	(filter (lambda (xs) (not (equal? xs '(0 0)))) crosses)))))


(define (manhatten pr)
  (let ((x (first pr))
	(y (second pr)))
    (+ (abs x) (abs y))))


(define (process-dist wires)
  ;;(first (sort (map manhatten (process wires)) <)))
  (apply min (map manhatten (process wires))))




#|
test cases 


(process '((R8 U5 L5 D3)(U7 R6 D4 L4)))
;; distance 6

(process '((R75 D30 R83 U83 L12 D49 R71 U7 L72)(U62 R66 U55 R34 D71 R55 D58 R83)))
;; distance 159

(process '((R98 U47 R26 D63 R33 U87 L62 D20 R33 U53 R51)(U98 R91 D20 R16 D67 R40 U7 R15 U6 R7)))
;; distance 135
|#


(process '((R8 U5 L5 D3)(U7 R6 D4 L4)))

(process-dist '((R8 U5 L5 D3)(U7 R6 D4 L4)))
(process-dist '((R75 D30 R83 U83 L12 D49 R71 U7 L72)(U62 R66 U55 R34 D71 R55 D58 R83)))
(process-dist '((R98 U47 R26 D63 R33 U87 L62 D20 R33 U53 R51)(U98 R91 D20 R16 D67 R40 U7 R15 U6 R7)))

(process-dist input)
;; 529

#|

(process-dist input)
529 

scheme@(guile-user) [18]> (sort (process input) (lambda (x y)(< (manhatten x)(manhatten y))))

$30 = ((-3 -526) (-3 -572) (-446 -301) (36 -861) (36 -865) (-415 -526)
(-90 -861) (-90 -865) (36 -1140) (-90 -1140) (956 355) (993 355) (1340
10) (-161 -1257) (1325 132) (1483 10) (1325 184) (81 -1460) (-437
-1257) (-609 -1166) (2019 10) (1335 814) (2019 -141) (1335 847) (2191
29) (2019 284) (1535 847) (2169 230) (1889 531) (2169 284) (1889 635)
(1889 684) (2125 531) (2440 230) (1861 817) (2152 531) (-1387 -1355)
(-1579 -1182) (-1520 -1245) (2125 684) (-1579 -1245) (2152 684) (-1387
-1480) (-1553 -1355) (-1745 -1182) (-1745 -1245) (2817 204) (-1553
-1480) (3066 22) (-1553 -1571) (3109 22) (3066 70) (3066 -558) (3109
-558) (-2327 -1571) (-2366 -1571))

scheme@(guile-user) [18]> (manhatten '(-3 -526))
$31 = 529

scheme@(guile-user) [18]> 
|#


(define (re-process-line xs pr)
  (let ((x 0)
	(y 0)
	(target-x (first pr))
	(target-y (second pr))
	(steps 0))
    (call/cc (lambda (escape)
	       (letrec ((check (lambda ()
				 (set! steps (+ 1 steps))
				 (when (and (= y target-y)
					    (= x target-x))
				   (escape steps))))
			(left (lambda (n) 
				(cond
				 ((< n 1) 'done)
				 (#t (set! x (- x 1))
				     (check)
				     (left (- n 1))))))
			(right (lambda (n) 
				 (cond
				  ((< n 1) 'done)
				  (#t (set! x (+ x 1))
				      (check)
				      (right (- n 1))))))
			(up (lambda (n) 
			      (cond
			       ((< n 1) 'done)
			       (#t (set! y (+ y 1))
				   (check)
				   (up (- n 1))))))
			(down (lambda (n) 
				(cond
				 ((< n 1) 'done)
				 (#t (set! y (- y 1))
				     (check)
				     (down (- n 1))))))
			(rec (lambda (xs) 
			       (cond
				((null? xs) 'done)
				(#t (let ((instruction (sym2op (car xs))))
				      (let ((op (car instruction))
					    (dist (cadr instruction)))
					(cond
					 ((eq? op 'left) (left dist))
					 ((eq? op 'right) (right dist))
					 ((eq? op 'up) (up dist))
					 ((eq? op 'down) (down dist))
					 (#t (error 'instruction (list op dist))))
					;; next instruction
					(rec (cdr xs)))))))))
		 (rec xs))))))

(re-process-line '(R8) '(3 0))

(re-process-line '(R8 U8) '(8 8))

(re-process-line '(R8 U5 L5 D2) '(3 3))

(re-process-line '(U7 R6 D2) '(6 5))



(define (re-process two-lines)
  (let ((line1 (first two-lines))
	(line2 (second two-lines))
	(crosses (process two-lines)))
    (let ((res 
	   (map (lambda (cross)
		  (let ((a (re-process-line line1 cross))
			(b (re-process-line line2 cross)))  
		    (list (+ a b)
			  a 
			  b
			  cross)))
		crosses)))
      (sort res (lambda (x y) (< (car x)(car y)))))))




(re-process '((R8 U5 L5 D3)(U7 R6 D4 L4)))
;;  ((30 15 15 (6 5)) (40 20 20 (3 3)))
;; 30

(re-process '((R75 D30 R83 U83 L12 D49 R71 U7 L72)(U62 R66 U55 R34 D71 R55 D58 R83)))
;;  ((610 206 404 (158 -12)) (624 290 334 (146 46)) (726 341 385 (155 4)) (850 472 378 (155 11)))
;; 610

(re-process '((R98 U47 R26 D63 R33 U87 L62 D20 R33 U53 R51)(U98 R91 D20 R16 D67 R40 U7 R15 U6 R7)))
;;  ((410 154 256 (107 47)) (516 207 309 (124 11)) (636 404 232 (107 71)) (650 301 349 (157 18)) (700 448 252 (107 51)))
;; 410

(re-process input)
;;  ((20386 1348 19038 (993 355)) (22054 2182 19872 (1335 847)) (33176 14175 19001 (956 355)) (33176 13337 19839 (1335 814)) (36098 23656 12442 (-90 -1140)) (36924 24203 12721 (-90 -861)) (37486 23782 13704 (36 -1140)) (37502 24077 13425 (36 -861)) (38428 25331 13097 (-3 -572)) (38888 22227 16661 (-446 -301)) (39404 27689 11715 (-609 -1166)) (39930 27952 11978 (-437 -1257)) (41268 24863 16405 (-415 -526)) (41278 25285 15993 (-3 -526)) (46010 31941 14069 (81 -1460)) (46266 33549 12717 (-90 -865)) (46266 34012 12254 (-161 -1257)) (46852 33423 13429 (36 -865)) (49468 44800 4668 (-1387 -1355)) (50404 45268 5136 (-1745 -1245)) (53082 45043 8039 (-1520 -1245)) (53082 46176 6906 (-2327 -1571)) (53136 48302 4834 (-1553 -1355)) (53866 48667 5199 (-1745 -1182)) (54320 45102 9218 (-1579 -1245)) (54320 44675 9645 (-1387 -1480)) (55766 48086 7680 (-1553 -1571)) (56544 49677 6867 (-2366 -1571)) (57656 48501 9155 (-1579 -1182)) (57656 48177 9479 (-1553 -1480)) (83428 5925 77503 (2019 284)) (83428 6406 77022 (1889 635)) (83850 6877 76973 (1889 684)) (89626 11696 77930 (2191 29)) (89626 11973 77653 (2169 284)) (89626 12500 77126 (1889 531)) (91690 2382 89308 (1535 847)) (91742 9842 81900 (3066 -558)) (92134 10191 81943 (3109 -558)) (92402 2738 89664 (1861 817)) (94088 7113 86975 (2125 684)) (94088 7882 86206 (2440 230)) (96546 5500 91046 (2019 -141)) (97228 7140 90088 (2152 684)) (98396 11919 86477 (2169 230)) (99060 5651 93409 (2019 10)) (99086 12264 86822 (2125 531)) (102478 12237 90241 (2152 531)) (103730 9262 94468 (3066 22)) (105282 10771 94511 (3109 22)) (107256 14715 92541 (1325 184)) (107634 14904 92730 (1340 10)) (110820 8285 102535 (2817 204)) (111366 9214 102152 (3066 70)) (112240 19367 92873 (1483 10)) (112240 19647 92593 (1325 132)))

;; 20386

;; accepted answer





    


      
    
    
  






































