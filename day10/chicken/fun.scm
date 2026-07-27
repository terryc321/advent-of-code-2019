;; chicken
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
   ((and (= dy 0) (>= dx 0)) 0.0)
   ((and (= dy 0) (< dx 0)) 180.0)
   ((and (= dx 0) (>= dy 0)) 90.0)
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

(define (check-square g x y)
  (let ((width (hash-table-ref g 'width))
	(height (hash-table-ref g 'height)))
    (when (and (>= x 0)(< x width)
	       (>= y 0)(< y height))
      (for px 0 (- width 1)
	   (for py 0 (- height 1)
		(let ((ang (angle (- px x)(- py y)))
		      (dist (pythagorus (- px x) (- py y))))
		  (format #t "checking ~a ~a : angle => ~a : dist => ~a ~%" px py ang dist)))))))
	
    




