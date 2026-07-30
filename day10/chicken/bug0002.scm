
#|

bug0001.scm

(fangle 1 -1)
south east region !
45.0000000000001

expect to be 90 + 45 or 135 degrees from north



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


;; cos sin tan all radian based computations
;; pi is 180 degrees 
(define pi 3.1415926535898d0)

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
    (format #t "south east region !~%")
    (let* ((a (/ pi 2))
	   (b (atan (/ dy dx)))
	   (c (+ a b))
	   (d 
	    (+ (/ pi 2)
	       (atan (/ dy dx)))))
      (list 'a a 'b b 'c c 'd d )))
   ;; bug0002 (angle 1 -1) 
   ;; now complains
   ;; Error: (*) bad argument type - not a number: (a 1.5707963267949 b -0.785398163397448 c 0.785398163397452 d 0.785398163397452)
   ;; ??

   ((and (< dx 0) (< dy 0)) ;;south-west region 
    (+ pi
       (atan (/ dx dy))))
   
   ((and (< dx 0) (> dy 0)) ;;north-west region 
    (+ (* 2 pi 3/4)
       (atan (/ dy dx))))
   
   (#t (error "north angle - should not reach here"))))


(define (angle dx dy)
  (radian->degree (north-angle dx dy)))

(define (radian->degree rad)
  (* rad (/ 180.0 pi)))

