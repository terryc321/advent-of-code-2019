

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


(assert (equal? '(right 123) (sym2op 'R123)))




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



    


      
    
    
  






































