
(defpackage #:fun
  (:use #:cl)
   (:shadow "STEP"))

(in-package #:fun)


#|
common -lisp

opcodes
1 = add
2 = multiply
99 = halt
? = error

|#

(defun mk-tape (xs)
  (make-array (length xs) :initial-contents xs))
  

(defun input ()
  (mk-tape '(1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 10 23 2 10 23 27 1 27 6 31 1 13 31 35 1 13 35 39 1 39 10 43 2 43 13 47 1 47 9 51 2 51 13 55 1 5 55 59 2 59 9 63 1 13 63 67 2 13 67 71 1 71 5 75 2 75 13 79 1 79 6 83 1 83 5 87 2 87 6 91 1 5 91 95 1 95 13 99 2 99 6 103 1 5 103 107 1 107 9 111 2 6 111 115 1 5 115 119 1 119 2 123 1 6 123 0 99 2 14 0 0)))


;; state of calc
;; position index i
;; t is true
;; nil is false 
(defun step (tape i)
  (let ((op (aref tape i)))
    (cond
      ((integerp op)
       (cond
	 ((= op 1) (step (exec-add tape i) (+ i 4)))
	 ((= op 2) (step (exec-mul tape i) (+ i 4)))
	 ((= op 99) tape)
	 (t (error "unknown integer opcode"))))
      (t (error "bad opcode")))))


(defun exec-add (tape i)
  (let ((index-1 (aref tape (+ i 1)))
	(index-2 (aref tape (+ i 2)))
	(index-3 (aref tape (+ i 3))))
    (let ((val1 (aref tape index-1))
	  (val2 (aref tape index-2))
	  (tape2 (copy-seq tape)))
      (setf (aref tape2 index-3) (+ val1 val2))
      ;; (format t "add ~a + ~a => ~a ~%" val1 val2 (+ val1 val2))
      ;; (format t "add tape = ~a ~%" tape)
      ;; (format t "add tape2 = ~a ~%" tape2)
      ;; (format t "~%~%")
      tape2)))


(defun exec-mul (tape i)
  (let ((index-1 (aref tape (+ i 1)))
	(index-2 (aref tape (+ i 2)))
	(index-3 (aref tape (+ i 3))))
    (let ((val1 (aref tape index-1))
	  (val2 (aref tape index-2))
	  (tape2 (copy-seq tape)))
      (setf (aref tape2 index-3) (* val1 val2))
      ;; (format t "mul ~a * ~a => ~a ~%" val1 val2 (* val1 val2))
      ;; (format t "mul tape = ~a ~%" tape)
      ;; (format t "mul tape2 = ~a ~%" tape2)
      ;; (format t "~%~%")
      tape2)))

      
(defun part-1 ()
  (let ((arr (copy-seq (input))))
    (setf (aref arr 1) 12)
    (setf (aref arr 2) 2)
    (step arr 0)))

	
;; some examples to run
(defun ex1 ()  (step (mk-tape '(1 0 0 0 99)) 0))
(defun ex2 ()  (step (mk-tape '(2 3 0 3 99)) 0))
(defun ex3 ()  (step (mk-tape '(2 4 4 5 99 0)) 0))
(defun ex4 ()  (step (mk-tape '(1 1 1 4 99 5 6 0 99)) 0))

(defun part-2 ()
  (let ((target 19690720))
    (loop for noun from 1 to 99 do
      (loop for verb from 1 to 99 do
	(let ((arr (copy-seq (input))))
	  (setf (aref arr 1) noun)
	  (setf (aref arr 2) verb)
	  (let ((out (step arr 0)))
	    (when (= target (aref out 0))
	      (let ((product (+ (* noun 100) verb)))
		(format t "solution noun ~a ... verb ~a... ~a ~%" noun verb product)))))))))



