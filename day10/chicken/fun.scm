;; chicken
(import (chicken sort)) ;; sorting?

(import (chicken format)) ;; format #t #f ...
(import srfi-1) ;; lists
(import srfi-69) ;; hash tables 
(import srfi-9)
;;(import srfi-13)
(import (chicken string)) ;; string-slpit
;; ,doc (chicken string string-split)
(import srfi-63) ;; 2d array make-array 
(import (chicken format))
(import (chicken process-context))
;;(current-directory)
;;(change-directory "day10/chicken")

;;2d array then
(define maxX 5)
(define maxY 7)
(define grid (make-array '#(#f) (+ maxX 1) (+ maxY 1)))

;; Access and modify elements
(array-set! grid "Hello" 0 0)
(array-set! grid "World" maxX maxY)

;; Retrieve values
(array-ref grid 0 0)    ;; => "Hello"
(array-ref grid maxX maxY) ;; => "World"

;; (define-syntax asteroid-string
;;   (syntax-rules ()
;;     ((_ s) (string-split s "\n" #f))))

;; have an asteroid grid type
;; width , height , some mechanism tell if there is an asteroid there or not
;; 

;; (define-record-type agrid
;;   (make-agrid w h elems)
;;   agrid?
;;   (w agrid-w)
;;   (h agrid-h)
;;   (elems agrid-elems agrid-elems!)
;;   ;;(w agrid-w set-agrid-w!)
;;   ;;(h agrid-h set-agrid-h!)
;;   )

;; (define (new-grid w h)
;;   (make-agrid w h (make-array '#(#f) (+ w 1) (+ y 1))))

;; (define a (make-agrid 5 7 (make-array '#(#f) (+ maxX 1) (+ maxY 1))))
;; (display (agrid-w a))
;; (display (agrid-h a))

;; (define p (make-point 5 10))
;; (display (point-x p))

;; (define-struct point (x y))

    ;; iterate over each list element
    ;; iterate over the string
(define (list-string-iterator xs proc)
  (letrec ((line-iterator (lambda (xs y)
			    (cond
			     ((null? xs) #f)
			     (#t (let ((line (car xs))
				       (rest (cdr xs)))
				   (string-iterator line 0 y)
				   (line-iterator rest (+ y 1)))))))
	   (string-iterator (lambda (s x y)
			      (cond
			       ((>= x (string-length s)) #f)
			       (#t (let ((ch (string-ref s x)))
				     (proc x y ch)
				     (string-iterator s (+ x 1) y)))))))
    (line-iterator xs 0)))
			  

(define (asteroid-string s)
  (let* ((xs (string-split s "\n" #f))
	 (width (string-length (first xs)))
	 (height (length xs))
	 (hash (make-hash-table test: equal? )))
    ;; iterate over each list element
    ;; iterate over the string
    (list-string-iterator xs (lambda (x y ch)
			       (cond
				((char=? ch #\#) (hash-table-set! hash (list x y) #t))
				(#t (hash-table-set! hash (list x y) #f)))))
				
    (hash-table-set! hash 'width width)
    (hash-table-set! hash 'height height)    
    hash 
  ))

(define example1  (asteroid-string 
"
.#..#
.....
#####
....#
...##
"))

(define example2  (asteroid-string
"
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
"))


(define example3 (asteroid-string
"
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
"))

(define example4 (asteroid-string
"
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
"))

(define example5 (asteroid-string
"
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
"))

(define input (asteroid-string
"
###..#########.#####.
.####.#####..####.#.#
.###.#.#.#####.##..##
##.####.#.###########
###...#.####.#.#.####
#.##..###.########...
#.#######.##.#######.
.#..#.#..###...####.#
#######.##.##.###..##
#.#......#....#.#.#..
######.###.#.#.##...#
####.#...#.#######.#.
.######.#####.#######
##.##.##.#####.##.#.#
###.#######..##.#....
###.##.##..##.#####.#
##.########.#.#.#####
.##....##..###.#...#.
#..#.####.######..###
..#.####.############
..##...###..#########
"))

;; everything really a hash table 
(define (show-grid g)
  (let ((width (hash-table-ref g 'width))
	(height (hash-table-ref g 'height)))
    (letrec ((helper (lambda (x y)
		       (cond
			((>= y height) #f)
			((>= x width)
			 (newline)
			 (helper 0 (+ y 1)))
			(#t (let ((elem (hash-table-ref g (list x y))))
			      (cond
			       (elem (format #t "#"))
			       (#t (format #t ".")))
			      (helper (+ x 1) y)))))))
      (helper 0 0))))

#|
where to put monitoring station ?
looking at map (0 , 0) to (width-1 , height-1) inclusive
so we need to score each position based on how many asteroids we can see

in terms of an angle deltaX deltaY

1,1 2,2 3,3 all multiples of 1,1
what largest angle ?
0,1
1,0

uni directional 0 to 360 ?
0 degrees or 0 radians on x , y plane
on an inverted y plane !

0,0  1,0  2,0 
0,1   

suppose i put monitoring station at some arbitrary (x,y) then whatever angle i create will
be (x+deltaX , y+deltaY)
delta X, Y can be negative also positive

generate all angles for all squares from (0,0) to (width-1,height-1) , we get a direction

;; delta is the angle ?!?
;; (define (angle x y x2 y2)
;;   (let ((deltaX (- x2 x))
;; 	(deltaY (- y2 y)))
;;     ;; angle (0,0) to (x3,y3)
;;     (

|#

(define (pythagorus dx dy)
  (sqrt (+ (* dx dx)
	   (* dy dy))))


;; cos sin tan all radian based computations
;; pi is 180 degrees 
(define pi 3.1415926535898d0)

(define (angle dx dy)
  (cond
   ((and (= dy 0) (= dx 0)) 480.0) ;; a dummy angle greater than any possible angle ?huh
   ((and (= dy 0) (> dx 0)) 0.0)
   ((and (= dy 0) (< dx 0)) 180.0)
   ((and (= dx 0) (> dy 0)) 90.0)
   ((and (= dx 0) (< dy 0)) 270.0)
   (#t (radian->degree (atan (/ dy dx))))))

(define (radian->degree rad)
  (* rad (/ 180.0 pi)))

;; iterate 1 to 10 say by increment 1
(define-syntax for
  (syntax-rules ()
    ((_ var from to body ...)
     (letrec ((foo (lambda (var)
		     (cond
		      ((> var to) #f)
		      (#t body ...
			  (foo (+ var 1)))))))
       (foo from)))))

;; (for i 1 10 (format #t "i = ~a ~%" i ))
;; (for i 1 10
;;      (for j 1 10 (format #t "i = ~a : j = ~a~%" i j )))

(define (float->int1000 x)
  (inexact->exact (floor (* 1000 x))))

(define-record-type angle&distance
  (make-angle&distance q a d p)
  angle&distance?
  (q angle&distance-quarter)  
  (a angle&distance-angle)
  (d angle&distance-distance)
  (p angle&distance-position))

(angle&distance? (make-angle&distance 1 12345 1000 123))
(angle&distance-quarter (make-angle&distance 1 12345 1000 123))
(angle&distance-angle (make-angle&distance 1 12345 1000 123))
(angle&distance-distance (make-angle&distance 1 12345 1000 (list 1 1)))
(angle&distance-position (make-angle&distance 1 12345 1000 (list 1 1)))

(define (quarter x y)
  (cond
   ((and (= x 0)(= y 0)) 1) ;; -- axes --- only consider distance ---
   ((and (= x 0)(> y 0)) 2) ;; east 
   ((and (= x 0)(< y 0)) 3) ;; west 
   ((and (> x 0)(= y 0)) 4) ;; north
   ((and (< x 0)(= y 0)) 5) ;; south
   ((and (> x 0)(> y 0)) 6) ;; north-east --- off axis -- consider both angle & distance ---
   ((and (< x 0)(> y 0)) 7) ;; north-west
   ((and (< x 0)(< y 0)) 8) ;; south-west
   ((and (> x 0)(< y 0)) 9) ;; south-east
   (else (error "quarter unhandled case"))))

(define (quarter->string n)
  (case n
    ((1) 'itself)
    ((2) 'east)
    ((3) 'west)
    ((4) 'north)
    ((5) 'south)
    ((6) 'north-east)
    ((7) 'north-west)
    ((8) 'south-west)
    ((9) 'south-east)
    (else (error "quarter->string"))))
    



(define (generate-data-for-square g x y)
  (let ((width (hash-table-ref g 'width))
	(height (hash-table-ref g 'height))
	(hash (make-hash-table)))
    (when (and (>= x 0)(< x width)
	       (>= y 0)(< y height))
      ;; iterate over all squares
      (let ((data '()))
	(for px 0 (- width 1)
	     (for py 0 (- height 1)
		  (when (hash-table-ref/default g (list px py) #f)
		    (let* ((delta-x (- px x))
			   (delta-y (- py y))
			   (quart (quarter delta-x delta-y))
			   (ang  (float->int1000 (angle delta-x delta-y)))
			   (dist (float->int1000 (pythagorus delta-x delta-y))))
		      (set! data (cons
				  (make-angle&distance quart ang dist (list px py))
				  data))			     
		      ;;(format #t "~a ~a : Q ~a : ang ~a : dist ~a ~%" px py quart ang dist)
		      ))))
	data))))

  

(define (sort-by-distance data)
  (sort data (lambda (x y)
	       (assert (angle&distance? x))
	       (assert (angle&distance? y))
	       (< (angle&distance-distance x)
		  (angle&distance-distance y)))))
		  

(define (quarterN-lowest-distance q data)
  (letrec ((fn (lambda (e)
		 (assert (angle&distance? e))
		 (= q (angle&distance-quarter e)))))
    (let ((result (sort-by-distance (filter fn data))))
      (cond
       ((null? result) result)
       (#t (list (first result)))))))

(define (quarter1-lowest-distance data)  (quarterN-lowest-distance 1 data))
(define (quarter2-lowest-distance data)  (quarterN-lowest-distance 2 data))
(define (quarter3-lowest-distance data)  (quarterN-lowest-distance 3 data))
(define (quarter4-lowest-distance data)  (quarterN-lowest-distance 4 data))
(define (quarter5-lowest-distance data)  (quarterN-lowest-distance 5 data))

(define-syntax dolist
  (syntax-rules ()
    ((_ (var xs) body ...)
     (letrec ((foo (lambda (ys)
		     (cond
		      ((null? ys) #f)
		      (#t (let ((var (car ys)))
			    body ...
			    (foo (cdr ys))))))))
       (foo xs)))))

;; (dolist (x '(1 2 3))
;; 	(format #t "x = ~a ~%" x))

;; (dolist (x '(1 2 3))
;; 	(dolist (y '(4 5 6))
;; 		(format #t "x = ~a : y = ~a~%" x y)))

(define (quarter-off-axis-lowest-distance q data)  
    (letrec ((fn (lambda (e)
		   (assert (angle&distance? e))
		   (= q (angle&distance-quarter e)))))
      (let* ((prelim (filter fn data))
	     (hash (make-hash-table test: equal?)))
	(dolist (p prelim)
		(let* ((key (angle&distance-angle p))
		       (dist (angle&distance-distance p))
		       (known (hash-table-ref/default hash key #f)))
		  (cond
		   (known (let ((dist2  (angle&distance-distance known)))
			    (when (< dist dist2)
			      (hash-table-set! hash key p))))
		   (else (hash-table-set! hash key p)))))
	;; hash should have all keys by angle at lowest possible distances
	(let ((result '()))
	  (hash-table-for-each hash (lambda (key value)
				      (set! result (cons value result))))
	  result))))

(define (quarter6-lowest-distance data)  (quarter-off-axis-lowest-distance 6 data))
(define (quarter7-lowest-distance data)  (quarter-off-axis-lowest-distance 7 data))
(define (quarter8-lowest-distance data)  (quarter-off-axis-lowest-distance 8 data))
(define (quarter9-lowest-distance data)  (quarter-off-axis-lowest-distance 9 data))

(define (all-quarters data)
  (letrec ((fn (lambda (e)
		 (assert (angle&distance? e))
		 (let ((dist (angle&distance-distance e)))
		   ;; (format #t "distance = ~a : ~a ~%" dist (zero? dist))
		   (not (zero? dist))))))
    (filter fn 
	    (append (quarter1-lowest-distance data)
		    (quarter2-lowest-distance data)
		    (quarter3-lowest-distance data)
		    (quarter4-lowest-distance data)
		    (quarter5-lowest-distance data)
		    ;; off axis
		    (quarter6-lowest-distance data)
		    (quarter7-lowest-distance data)
		    (quarter8-lowest-distance data)
		    (quarter9-lowest-distance data)))))


(define (scruples g x y)
  (let ((data (generate-data-for-square g x y)))
    (all-quarters data)))

  
(define (show-angle&distance s)
  (format #t "quarter ~a: angle ~a : distance ~a : position ~a ~%"
	  (quarter->string (angle&distance-quarter s))
	  (angle&distance-angle s)
	  (angle&distance-distance s)
	  (angle&distance-position s)))

(define (padding i)
  (let ((s (format #f "~a" i)))
    (cond
     ((= (string-length s) 1) (format #f "00~a" s))
     ((= (string-length s) 2) (format #f "0~a" s))
     ((= (string-length s) 3) (format #f "~a" s))
     (else (error "padding")))))
   

(define (compute-scruples-grid g)
  (let ((width (hash-table-ref g 'width))
	(height (hash-table-ref g 'height))
	(best 0)
	(best-position '(0 0)))
    (for y 0 (- height 1)
	 (format #t "~%")
	 (for x 0 (- width 1)
	      (let ((len (length (scruples g x y))))
		(format #t "~a " (padding len))
		(when (and (hash-table-ref/default g (list x y) #f)
			   (> len best))
		  (set! best len)
		  (set! best-position (list x y))))))
    (format #t "~%")
    (format #t "best was ~a at position ~a ~%" best best-position)
    best))
	  
(define (compute-scruples-grid-xy g x y)
  (let ((result (scruples g x y)))
    (dolist (e result)
	    (show-angle&distance e))))
	       

;; tests
;; (compute-scruples-grid example1)
;; (compute-scruples-grid example2)
;; (compute-scruples-grid example3)
;; (compute-scruples-grid example4)
;; (compute-scruples-grid example5)

(define (part1)
  (compute-scruples-grid input))
;;best was 221 at position (11 11) 





