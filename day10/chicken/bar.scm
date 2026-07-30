

#|
bar.scm

- fun.scm solved for input at coordinate ?,?
best was 221 at position (11 11) 
221
input is 21 x 21 position 11 11 from 0,0 to 20,20 inclusive

in part2 we wanted to get Y axis as being up , X axis as right , so a normal
math orientation

dont know what un-intended consequences this has for the solution - may not be possible
to recover the answer due to confusion over which orientation we are working on

determine the 200th asteroid to be obliterated

-- keychron c3 ?? already have keychron c2

know where asteroids are , we can search  , or?
given laser is at x , y - presumably we ignore the asteroid already at x , y ?
set of positions , we need to determine the angle from north that the asteroid
creates , from this we can sort them ,
then we need to sort based on distance from the laser ,
so we can compile a list of the asteroids destroyed in order

vaporising with a giant laser 
|#

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

;; while
(define-syntax while
  (syntax-rules ()
    ((_ con body ...)
     (letrec ((foo (lambda ()
		     (when con
		       body ...
		       (foo)))))
       (foo)))))

(define (test-while)
  (let ((i 0))
    (while #t (format #t "hello world ~a~%" i)
	   (set! i (+ i 1)))))


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


(define-syntax downfor
  (syntax-rules ()
    ((_ var from to body ...)
     (letrec ((foo (lambda (var)
		     (cond
		      ((< var to) #f)
		      (#t body ...
			  (foo (- var 1)))))))
       (foo from)))))

;; (downfor i 10 1 (format #t "i = ~a ~%" i ))
;; (downfor i 10 1  (downfor j 10 1 (format #t "i = ~a : j = ~a~%" i j )))

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

;; list iterator - not required now 
;;     ;; iterate over each list element
;;     ;; iterate over the string
;; (define (list-string-iterator xs proc)
;;   (letrec ((line-iterator (lambda (xs y)
;; 			    (cond
;; 			     ((null? xs) #f)
;; 			     (#t (let ((line (car xs))
;; 				       (rest (cdr xs)))
;; 				   (string-iterator line 0 y)
;; 				   (line-iterator rest (+ y 1)))))))
;; 	   (string-iterator (lambda (s x y)
;; 			      (cond
;; 			       ((>= x (string-length s)) #f)
;; 			       (#t (let ((ch (string-ref s x)))
;; 				     (proc x y ch)
;; 				     (string-iterator s (+ x 1) y)))))))
;;     (line-iterator xs 0)))



(define (asteroid-string s)
  (let* ((xs (string-split s "\n" #f))
	 (width (string-length (first xs)))
	 (height (length xs))
	 (hash (make-hash-table test: equal? )))
    ;; iterate over each list element
    ;; iterate over the string
    (for y 0 (- height 1)
	     (for x 0 (- width 1)
		  (let ((ch (string-ref (list-ref xs (- height 1 y)) x)))
		    (if (char=? ch #\#)
			(hash-table-set! hash (list x y) #t)
			(hash-table-set! hash (list x y) #f)))))
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

(define part2-example1 (asteroid-string
"
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.........###..
..#.#.....#....##
"))
  ;; X at 8 1 in (0,0) bottom left coordiantes 

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

;; show the grid from largest Y to lowest Y , from lowest X to largest X 
(define (show-grid g)
  (let ((width (hash-table-ref g 'width))
	(height (hash-table-ref g 'height)))
    (downfor y (- height 1) 0
	     (format #t "~%")
	     (for x 0 (- width 1)
		  (let ((elem (hash-table-ref/default g (list x y) #f)))
		    (cond
		     (elem
		      (cond
		       ((equal? elem #t) (format #t "#"))
		       (else (format #t "~a" elem))))
		     (#t (format #t "."))))))
    (format #t "~%")
    #f))

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


|#

(define (pythagorus dx dy)
  (sqrt (+ (* dx dx)
	   (* dy dy))))


;; cos sin tan all radian based computations
;; pi is 180 degrees 
(define pi 3.1415926535898d0)

;; blessed 
(define (north-angle dx dy)
  (cond
   ((and (= dy 0) (= dx 0)) (error "angle undefined - it is the same position"))
   ;; up/down
   ((and (= dx 0) (> dy 0)) 0) ;; north 
   ((and (= dx 0) (< dy 0)) pi) ;; south 180 degrees
   ;; left/right 
   ((and (= dy 0) (> dx 0)) (/ pi 2)) ;; east 90 degrees
   ((and (= dy 0) (< dx 0)) (* 2 pi 3/4)) ;; west 270.0 degrees
   
   ((and (> dx 0) (> dy 0)) ;;north-east region 
    (atan (/ dx dy)))

   ((and (> dx 0) (< dy 0)) ;;south-east region
    (+ (/ pi 2)
       (abs (atan (/ dy dx)))))

   ((and (< dx 0) (< dy 0)) ;;south-west region 
    (+ pi
       (abs (atan (/ dx dy)))))
   
   ((and (< dx 0) (> dy 0)) ;;north-west region 
    (+ (* 2 pi 3/4)
       (abs (atan (/ dy dx)))))
   
   (#t (error "north angle - should not reach here"))))



;;for a given asteroid puzzle - find all the asteroids
(define (radian->degree rad)
  (* rad (/ 180.0 pi)))

(define (float->int1000 x)
  (inexact->exact (floor (* 1000 x))))

;; the angle multiplied by 1000 and rounded off , hopefully sufficient precision 
(define (angle dx dy)
  (float->int1000 (radian->degree (north-angle dx dy))))

(define-record-type angle&distance
  (make-angle&distance a d p i)
  angle&distance?
  (a angle&distance-angle)
  (d angle&distance-distance)
  (p angle&distance-position)
  (i angle&distance-inverted))


;; simplest mechanism to collect known asteroids 
(define (compute-asteroids g x0 y0)
  (let ((width (hash-table-ref g 'width))
	(height (hash-table-ref g 'height))
	(asteroids '()))
    (for y 0 (- height 1)
	 (for x 0 (- width 1)	      
	      (when (and (not (and (= x x0)(= y y0)))
			 (hash-table-ref/default g (list x y) #f))
		(let* ((delta-x (- x x0))
		       (delta-y (- y y0))
		       (ang (angle delta-x delta-y))
		       (dist (pythagorus delta-x delta-y))
		       (asteroid (make-angle&distance ang dist (list x y) (list x (- height 1 y )))))
		  (set! asteroids (cons asteroid asteroids))))))
    asteroids))

(define (count-asteroids g)
  (let ((width (hash-table-ref g 'width))
	(height (hash-table-ref g 'height))
	(count 0))
    (for y 0 (- height 1)
	 (for x 0 (- width 1)	      
	      (when (hash-table-ref/default g (list x y) #f)
		(set! count (+ count 1)))))
    count))


;; (make-laser 
(define (make-laser ex x y)
  (let* ((asteroids (sort (compute-asteroids ex x y)
			  (lambda (a b)
			    (< (angle&distance-angle a)
			       (angle&distance-angle b)))))
	 (angle -90)
	 (fire-count 0)
	 (len-asteroids (length asteroids))
	 (status #t))
    (format #t "making a laser with ~a asteroids at (~a,~a ) ~%"  len-asteroids x y)
    (lambda (op)
      (cond
         ((eq? op 'ok) status)
	 ((eq? op 'fire)  ;; those with angle greater than current angle 
	  (let ((candidates (filter (lambda (e)  (> (angle&distance-angle e) angle)) asteroids)))
	    (when (null? candidates)
	      ;;(format #t "no more candidates~%")	      
	      ;; (set! status #f)
	      ;; #f)
	      (set! candidates (sort asteroids (lambda (a b)
						 (< (angle&distance-angle a)
						    (angle&distance-angle b))))))
	    ;; 
	     (let ((alpha (car candidates)))
		   ;; for this given angle - find all candidates
		   (let* ((alpha-angle (angle&distance-angle alpha))
			  (candidates2 (filter (lambda (e)
						(= (angle&distance-angle e) alpha-angle))
				    asteroids))
	  	     ;; we have all candidates on this angle now
		     ;; sort them based on distance
			  (dist-cand (sort candidates2 (lambda (a b)
							 (< (angle&distance-distance a)
							    (angle&distance-distance b))))))
		     (cond
		      ((null? dist-cand)
		       (format #t "null dist-cand?~%")
		       (set! status #f)
		       #f)
		      (#t (let ((chosen (first dist-cand)))
			    (set! fire-count (+ fire-count 1))
			    (set! angle alpha-angle)
			    (format #t "fired ~a on " fire-count)
			    (show-angle&distance chosen)
			    (set! len-asteroids (length asteroids))
			    (set! asteroids (remove (lambda (e) (equal? e chosen))
						    asteroids))
			    (set! asteroids (sort asteroids (lambda (a b)
							 (< (angle&distance-angle a)
							    (angle&distance-angle b)))))
			    (assert (= (length asteroids) (+ -1 len-asteroids)))
			    chosen)))))))
	 (#t (error "bad laser"))))))


(define (copy-grid ex)
  (let* ((result (make-hash-table test: equal?))
	 (width (hash-table-ref ex 'width))
	 (height (hash-table-ref ex 'height)))
    (hash-table-set! result 'width width)
    (hash-table-set! result 'height height)    
    (downfor y (- height 1) 0
	     (for x 0 (- width 1)
		  (let ((elem (hash-table-ref/default ex (list x y) #f)))
		    (when elem
		      (hash-table-set! result (list x y) elem)))))
	 result))

  


(define-syntax grid-fires
  (syntax-rules ()
    ((_ ex p c1 message) (begin
		       (set! c1 (copy-grid ex))
		       (dolist (v '(1 2 3 4 5 6 7 8 9))
			       (when (p 'ok)
				 (let ((asteroid (p 'fire)))
				   (when asteroid 
				     (let* ((position (angle&distance-position asteroid))
					    (x (first position))
					    (y (second position)))
				       (hash-table-set! c1 position v)
				       )))))
		       (format #t "~a ...~%" message)
		       (show-grid c1)))))



(define (test-part2-example)
  (let* ((ex part2-example1)
	 (p (make-laser ex 8 1))
	 (c1 '()))
    (grid-fires ex p c1 "first 9 fires")
    (grid-fires ex p c1 "second 9 fires")
    (grid-fires ex p c1 "third 9 fires")
    (grid-fires ex p c1 "fourth 9 fires")
    ))




(define (inverted-position g x y)
  (let ((height (hash-table-ref g 'height)))
    (list x (- height 1 y))))


#|

.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##

X inverted coords 0,0 at topleft X has position (8,3)
math coords 0,0 at bottomleft X has position (8,1)

(inverted-position part2-example1 8 1)
(8 3)


from fun.scm solution was at inverted coordinates (11,11) => (11 9) math coord 
use input 11 9
obtain solutions ...

the 200th solution has math coordiante ...
inverted to give ...

final part answer is ... 

|#
(define (test-part2)
  (let* ((p (make-laser input 11 9)))	 
    (while (p 'ok)
      (p 'fire))))

;; fired 200 on angle 329036 : distance 5.8309518948453 : position (8 14)
;; (inverted-position input 8 14)
;; (8 6)

;; 8 * 100 + 6 = 806 
;; 806 accepted answer



;; sort based on angle 


;; (define (compute-asteroids2 g x0 y0)
;;   (let ((width (hash-table-ref g 'width))
;; 	(height (hash-table-ref g 'height))
;; 	(asteroids (make-hash-table test: equal?))
;; 	(angles '()))
;;     (for y 0 (- height 1)
;; 	 (for x 0 (- width 1)	      
;; 	      (when (and (not (and (= x x0)(= y y0)))
;; 			 (hash-table-ref/default g (list x y) #f))
;; 		(let* ((delta-x (- x x0))
;; 		       (delta-y (- y y0))
;; 		       (ang (angle delta-x delta-y))
;; 		       (dist (pythagorus delta-x delta-y))
;; 		       (asteroid (make-angle&distance ang dist (list x y))))
;; 		  ;; put asteroid into asteroid hash table
;; 		  (let ((val (hash-table-ref/default asteroids ang #f)))
;; 		    (cond
;; 		     (val (hash-table-set! asteroids ang (cons asteroid val)))
;; 		     (#t (hash-table-set! asteroids ang (list asteroid)))))))))

;;     ;; for each
;;     (hash-table-for-each asteroids
;; 			 (lambda (k v)
;; 			   (set! angles (cons k angles))))
    
;;     ;; sort the angles
;;     (set! angles (sort angles <))
    
;;     ;; iterate over the angles
;;     (format #t "angles ~a ~%" angles )

;;     ;;
;;     (let ((lasered 0))
;;      (call/cc (lambda (exit)
;;      (while #t 
;;       ;; angles are all sorted so iterate over the available angles in sequence
;;       ;; remove any angles that have zero entries ?? or just ignore them ??
;;       ;; or count number of items removed when < 1 we are done ?
;;       ;; or if we counted 200 asteroids just call exit ?
;;       (dolist (ang angles)
;; 	      (let ((known (hash-table-ref/default asteroids ang #f)))

;; 		(cond
;; 		 ((null? known) #f)
;; 		 (#t
;; 		  (when (> (length known) 1)
;; 		    (set! known (sort known (lambda (a b)
;; 					      (< (angle&distance-distance a)
;; 						 (angle&distance-distance b)))))
		    
;; 		    (let ((chosen (first known))
;; 			  (the-rest (cdr known)))
;; 		      (hash-table-set! asteroids ang the-rest)
;; 		      (set! lasered (+ lasered 1))
;; 		      (format #t "lasered ~a on ~a  ~%" chosen lasered)
;; 		      (when (= lasered 200)
;; 			(exit #t))))))))))))))




;; (angle&distance? (make-angle&distance 1 12345 1000 123))
;; (angle&distance-quarter (make-angle&distance 1 12345 1000 123))
;; (angle&distance-angle (make-angle&distance 1 12345 1000 123))
;; (angle&distance-distance (make-angle&distance 1 12345 1000 (list 1 1)))
;; (angle&distance-position (make-angle&distance 1 12345 1000 (list 1 1)))

;; (define (quarter x y)
;;   (cond
;;    ((and (= x 0)(= y 0)) 0) ;; itself 
;;    ((and (> x 0)(= y 0)) 1) ;; north
;;    ((and (> x 0)(> y 0)) 2) ;; north-east --- off axis -- consider both angle & distance ---
;;    ((and (= x 0)(> y 0)) 3) ;; east
;;    ((and (> x 0)(< y 0)) 4) ;; south-east
;;    ((and (< x 0)(= y 0)) 5) ;; south
;;    ((and (< x 0)(< y 0)) 6) ;; south-west   
;;    ((and (= x 0)(< y 0)) 7) ;; west 
;;    ((and (< x 0)(> y 0)) 8) ;; north-west
;;    (else (error "quarter unhandled case"))))

;; (define (quarter->string n)
;;   (case n
;;     ((0) 'itself? )    
;;     ((1) 'north)
;;     ((2) 'north-east)
;;     ((3) 'east)
;;     ((4) 'south-east)
;;     ((5) 'south)
;;     ((6) 'south-west)
;;     ((7) 'west)
;;     ((8) 'north-west)
;;     (else (error "quarter->string"))))
    


  

;; (define (sort-by-distance data)
;;   (sort data (lambda (x y)
;; 	       (assert (angle&distance? x))
;; 	       (assert (angle&distance? y))
;; 	       (< (angle&distance-distance x)
;; 		  (angle&distance-distance y)))))
		  

;; (define (quarterN-lowest-distance q data)
;;   (letrec ((fn (lambda (e)
;; 		 (assert (angle&distance? e))
;; 		 (= q (angle&distance-quarter e)))))
;;     (let ((result (sort-by-distance (filter fn data))))
;;       (cond
;;        ((null? result) result)
;;        (#t (list (first result)))))))

;; (define (quarter1-lowest-distance data)  (quarterN-lowest-distance 1 data))
;; (define (quarter2-lowest-distance data)  (quarterN-lowest-distance 2 data))
;; (define (quarter3-lowest-distance data)  (quarterN-lowest-distance 3 data))
;; (define (quarter4-lowest-distance data)  (quarterN-lowest-distance 4 data))
;; (define (quarter5-lowest-distance data)  (quarterN-lowest-distance 5 data))


;; (define (quarter-off-axis-lowest-distance q data)  
;;     (letrec ((fn (lambda (e)
;; 		   (assert (angle&distance? e))
;; 		   (= q (angle&distance-quarter e)))))
;;       (let* ((prelim (filter fn data))
;; 	     (hash (make-hash-table test: equal?)))
;; 	(dolist (p prelim)
;; 		(let* ((key (angle&distance-angle p))
;; 		       (dist (angle&distance-distance p))
;; 		       (known (hash-table-ref/default hash key #f)))
;; 		  (cond
;; 		   (known (let ((dist2  (angle&distance-distance known)))
;; 			    (when (< dist dist2)
;; 			      (hash-table-set! hash key p))))
;; 		   (else (hash-table-set! hash key p)))))
;; 	;; hash should have all keys by angle at lowest possible distances
;; 	(let ((result '()))
;; 	  (hash-table-for-each hash (lambda (key value)
;; 				      (set! result (cons value result))))
;; 	  result))))

;; (define (quarter6-lowest-distance data)  (quarter-off-axis-lowest-distance 6 data))
;; (define (quarter7-lowest-distance data)  (quarter-off-axis-lowest-distance 7 data))
;; (define (quarter8-lowest-distance data)  (quarter-off-axis-lowest-distance 8 data))
;; (define (quarter9-lowest-distance data)  (quarter-off-axis-lowest-distance 9 data))

;; (define (all-quarters data)
;;   (letrec ((fn (lambda (e)
;; 		 (assert (angle&distance? e))
;; 		 (let ((dist (angle&distance-distance e)))
;; 		   ;; (format #t "distance = ~a : ~a ~%" dist (zero? dist))
;; 		   (not (zero? dist))))))
;;     (filter fn 
;; 	    (append (quarter1-lowest-distance data)
;; 		    (quarter2-lowest-distance data)
;; 		    (quarter3-lowest-distance data)
;; 		    (quarter4-lowest-distance data)
;; 		    (quarter5-lowest-distance data)
;; 		    ;; off axis
;; 		    (quarter6-lowest-distance data)
;; 		    (quarter7-lowest-distance data)
;; 		    (quarter8-lowest-distance data)
;; 		    (quarter9-lowest-distance data)))))


;; (define (scruples g x y)
;;   (let ((data (generate-data-for-square g x y)))
;;     (all-quarters data)))

  
(define (show-angle&distance s)
  (format #t "angle ~a : distance ~a : position ~a : inverted ~a ~%"
	  (angle&distance-angle s)
	  (angle&distance-distance s)
	  (angle&distance-position s)
	  (angle&distance-inverted s)))
	  
	  

;; (define (padding i)
;;   (let ((s (format #f "~a" i)))
;;     (cond
;;      ((= (string-length s) 1) (format #f "00~a" s))
;;      ((= (string-length s) 2) (format #f "0~a" s))
;;      ((= (string-length s) 3) (format #f "~a" s))
;;      (else (error "padding")))))
   

;; (define (compute-scruples-grid g)
;;   (let ((width (hash-table-ref g 'width))
;; 	(height (hash-table-ref g 'height))
;; 	(best 0)
;; 	(best-position '(0 0)))
;;     (for y 0 (- height 1)
;; 	 (format #t "~%")
;; 	 (for x 0 (- width 1)
;; 	      (let ((len (length (scruples g x y))))
;; 		(format #t "~a " (padding len))
;; 		(when (and (hash-table-ref/default g (list x y) #f)
;; 			   (> len best))
;; 		  (set! best len)
;; 		  (set! best-position (list x y))))))
;;     (format #t "~%")
;;     (format #t "best was ~a at position ~a ~%" best best-position)
;;     best))
	  
;; (define (compute-scruples-grid-xy g x y)
;;   (let ((result (scruples g x y)))
;;     (dolist (e result)
;; 	    (show-angle&distance e))))
	       

;; ;; tests
;; ;; (compute-scruples-grid example1)
;; ;; (compute-scruples-grid example2)
;; ;; (compute-scruples-grid example3)
;; ;; (compute-scruples-grid example4)
;; ;; (compute-scruples-grid example5)

;; (define (part1)
;;   (compute-scruples-grid input))
;; ;;best was 221 at position (11 11) 






