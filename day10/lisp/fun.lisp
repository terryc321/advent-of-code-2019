(ql:quickload :fiveam)
(ql:quickload :uiop)

(uiop:define-package :fun
    (:use :cl))
(in-package :fun)

#|

#.........
...A......
...B..a...
.EDCG....a
..F.c.b...
.....c....
..efd.c.gb
.......c..
....f...c.
...e..d..c

asteroid at (0,0)
A 3,1
B 3,2
C 3,3
D 2,3
E 1,3
F 2,4
G 4,3

width 0 .. 9
height 0 .. 9 

reduce to lowest multiples

|#

(defun create-fn (ox oy sym sym2)
  (lambda (x y)
    (multiple-value-bind (k1 r1) (floor x ox)
      (multiple-value-bind (k2 r2) (floor y oy)
	(cond
	  ((and (= x ox)(= y oy)) sym)
	  ((and (= x 0)(= y 0)) nil)	  
	  ((and (= k1 k2) (= r1 0)(= r2 0)) sym2)
	  (t nil))))))



(defparameter a (create-fn 3 1 "A" "a"))
(defparameter b (create-fn 3 2 "B" "b"))
(defparameter c (create-fn 3 3 "C" "c"))
(defparameter d (create-fn 2 3 "D" "d"))
(defparameter e (create-fn 1 3 "E" "e"))
(defparameter f (create-fn 2 4 "F" "f"))
(defparameter g (create-fn 4 3 "G" "g"))

(defun show-grid ()
  (format t "~%")
  (loop for y from 0 to 9 do
    (format t "~%")
    (loop for x from 0 to 9 do
      (let* ((xs (list a b c d e f g))
	     (out (map 'list (lambda (f) (funcall f x y)) xs))
	     (out2 (remove-if (lambda (p) (eq p nil)) out))
	     (len2 (length out2)))
	(cond
	  ((= len2 0) (format t "."))
	  ((= len2 1) (format t "~a" (car out2)))
	  (t (format t "~%multiple ! ~a~%" out2)
	     (error "foo"))))))
  (format t "~%"))




	
      
	
