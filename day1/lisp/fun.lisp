
;; read from a file
(defpackage :aoc
  (:use :cl))
(in-package :aoc)

(defparameter input
  (with-open-file (stream "../input" :direction :input) 
    (read stream)))


(defun mass (n)  (- (floor (/ n 3)) 2))

#|
For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
For a mass of 1969, the fuel required is 654.
For a mass of 100756, the fuel required is 33583.
|#

;;(mapcar #'mass '(12 14 1969 100756))

(defun part1 ()
  (apply #'+ (mapcar #'mass input)))

(part1)
3497399

