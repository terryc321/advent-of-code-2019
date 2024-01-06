
#|
day 2 aoc 2019 
back in common lisp lem editor

how fix lem key bindings ?


|#

(declaim (optimize (speed 0) (debug 3) (safety 3)))


(defpackage :aoc
  (:use :cl :str))
(in-package :aoc)

(defparameter in "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0")

(defun fresh-in ()
  (let ((res in))
;; split comma seperated values
(setq res (split "," res))

;; parse each string as an integer
(setq res (mapcar #'parse-integer res))

;; coerce to vector
(setq res (coerce res 'vector))
res
))




;; (aref in 0)
;; ;; now what?

;; (length in)

#|
99 means halt
1 means A B C
read values from index A and B , add , store result in C , advance 4 indexs

|#

(defun run (vec)
  (let ((i 0)(ins nil)(HALT 99)(ADD 1)(MULT 2))
    (catch 'done
    (loop while t do
          (setq ins (aref vec i))
          (cond
            ((= ins HALT) (throw 'done t))
            ((= ins ADD) (let* ((a (aref vec (aref vec (+ i 1))))
                                (b (aref vec (aref vec (+ i 2))))
                                (sum (+ a b)))
                           (setf (aref vec (aref vec (+ i 3))) sum)
                           (setq i (+ i 4))))
            ((= ins MULT) (let* ((a (aref vec (aref vec (+ i 1))))
                                (b (aref vec (aref vec (+ i 2))))
                                (prod (* a b)))
                           (setf (aref vec (aref vec (+ i 3))) prod)
                           (setq i (+ i 4))))
            
            (t (error "run command not understood" ins)))))))

#|
replace position 1 with 12
replace position 2 with 2

what value is left at position 0 ?
|#

(defun part1()
    (let ((in2 (fresh-in)))
      (setf (aref in2 1) 12)
      (setf (aref in2 2) 2)
      (run in2)
      (aref in2 0)))

;; make a copy of vector so as not to 
(defun vec-copy (v)
    (let ((arr (make-array (length v) :initial-contents v)))
      arr))


#|
part 2

noun 0 .. 99 replace position 1 with chosen value
verb 0 .. 99 replace position 2 with chosen value
desired output 19690720

vector copy so do not trash original vector with past experiments
|#

(defun part2 ()
  (let ((v (fresh-in))
        (TARGET 19690720))
  (loop for noun from 0 to 99 do
        (loop for verb from 0 to 99 do
              (let ((v2 (vec-copy v)))
                (setf (aref v2 1) noun)
                (setf (aref v2 2) verb)
                (run v2)
                (cond
                  ((= (aref v2 0) TARGET)
                   (format t "noun - verb : [~a] [~a] ~%" noun verb))))))))

;; noun - verb : [64] [21]

                  








          






















