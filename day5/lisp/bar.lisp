

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;; (defparameter plist '(0 1 2 3 4 5))
;; (setf (nth 2 plist) 22)
;; plist


(defpackage :aoc
  (:use :cl))
(in-package :aoc)

(defun fresh-in ()
  (with-open-file (stream "../input" :direction :input)
    (read stream)))

;; diagnostic thermal radiator controller = 5 
(defun computer-input ()
  5)

(defun computer-output (r)
  (format t "OUTPUT { ~a } ~%" r))

(defun config (r)
  (let ((op (mod r 100))
	(c  (floor (mod r 1000) 100))
	(b  (floor (mod r 10000) 1000))
	(a  (floor (mod r 100000) 10000)))
    (values op a b c)))


;; (multiple-value-bind (op a b c) (config 54321)
;;   (list 'op op 'a a 'b b 'c c))

;; (multiple-value-bind (op a b c) (config 1002)
;;   (list 'op op 'a a 'b b 'c c))




(defparameter *vector* nil)	


(defun decode-op (r)
  (case r
    (99 'halt)
    (1 'add)
    (2 'mult)
    (3 'read)
    (4 'write)
    (5 'jump-if-true)
    (6 'jump-if-false)
    (7 'less-than)
    (8 'equals)
    (t (error "wtf unrecognised decode op ! "))))
  

(defun show-vector (vec i)
  (let ((vlast (- (length vec) 1)))
    (loop for j from 0 to vlast do
      (let ((n (aref vec j)))
	(cond
	  ((= i j) (format t "{~a} " n))
	  (t (format t "~a " n)))))
    (format t "~%")))


  


(defun run (vec finput)
  (setq *vector* vec)
  (let ((i 0)(ins nil)
	(HALT 99)
	(ADD 1)
	(MULT 2)
	(READ 3)
	(WRITE 4)
	(JUMP-IF-TRUE 5) 
	(JUMP-IF-FALSE 6) 
	(LESS-THAN 7) ;; todo
	(EQUALS 8) ;; todo
	)
    (catch 'done
      (loop while t do
        (setq ins (aref vec i))
	(multiple-value-bind (op pa pb pc) (config ins)
	  (format t "~%")
	  ;;(format t "VECTOR : ~a ~%" vec)
	  ;; (show-vector vec i)
	  ;; (format t "POOR : OP ~a (~a): ~a ~a ~a ~%" (decode op) op pa pb pc)
	  ;;(format t "POOR : PARAMS ~a ~a ~a ~%" a b c)
		 
            (cond
              ((= op HALT)
	       (format t "POOR : HALT !! ~%")
	       (throw 'done t))

	      ((= op WRITE)
	       (let ((a (cond ;; pc mode of 1st parameter
			  ((= pc 0) (aref vec (aref vec (+ i 1))))
			  ((= pc 1) (aref vec (+ i 1)))
			  (t (error "bad PC on WRITE")))))
		 (computer-output a)
		 (format t "POOR : PARAMS ~a ~%" a)
		 (format t "WRITE ~a ~%" a)
                 (setq i (+ i 2))))
	      
	      ((= op READ)
	       (let* ((a (cond ;; pc mode of 1st parameter
			   ((= pc 0) (aref vec (aref vec (+ i 1))))
			   ((= pc 1) (aref vec (+ i 1)))
			   (t (error "bad PC on READ"))))
                      (b (funcall finput)))
                 ;;(setf (aref vec (aref vec (+ i 1))) b)
		 (setf (aref vec (aref vec (+ i 1))) b)
		 (format t "POOR : PARAMS ~a ~a ~%" a b)		 
		 (format t "READ {~a} <- ~a ~%" a b)
		 (format t "CHECK at ~a is ~a ~%"
			 (aref vec (+ i 1))
			 (aref vec (aref vec (+ i 1))))
                 (setq i (+ i 2))))
              
              ((= op JUMP-IF-FALSE) ;; OP A B 
	       (let* ((a (cond ;; pc mode of 1st parameter
			   ((= pc 0) (aref vec (aref vec (+ i 1))))
			   ((= pc 1) (aref vec (+ i 1)))
			   (t (error "bad PC on JUMP-IF-FALSE"))))
                      (b (cond ;; pb mode of 2nd parameter
			   ((= pb 0) (aref vec (aref vec (+ i 2))))
			   ((= pb 1) (aref vec (+ i 2)))
			   (t (error "bad PB on JUMP-IF-FALSE")))))
                 (cond
		   ((zerop a)
		    (format t "JUMP-IF-FALSE activated to jumping to ~a" b)
		    (setq i b))
		   (t
		    (format t "JUMP-IF-FALSE skipped")		    
		    (setq i (+ i 3))))))
	      
	      
	      ((= op JUMP-IF-TRUE) ;; OP A B
	       (let* ((a (cond ;; pc mode of 1st parameter
			   ((= pc 0) (aref vec (aref vec (+ i 1))))
			   ((= pc 1) (aref vec (+ i 1)))
			   (t (error "bad PC on JUMP-IF-TRUE"))))
                      (b (cond ;; pb mode of 2nd parameter
			   ((= pb 0) (aref vec (aref vec (+ i 2))))
			   ((= pb 1) (aref vec (+ i 2)))
			   (t (error "bad PB on JUMP-IF-TRUE")))))
		 (cond
		   ((zerop a)
		    (format t "JUMP-IF-TRUE skipped")		    		    
		    (setq i (+ i 3)))
		   (t
		    (format t "JUMP-IF-TRUE activated to jumping to ~a" b)
		    (setq i b)))))


	      ((= op LESS-THAN) ;; OP A B C 
	       (let* ((a (cond ;; pc mode of 1st parameter
			   ((= pc 0) (aref vec (aref vec (+ i 1))))
			   ((= pc 1) (aref vec (+ i 1)))
			   (t (error "bad PC on LESS-THAN"))))
                      (b (cond ;; pb mode of 2nd parameter
			   ((= pb 0) (aref vec (aref vec (+ i 2))))
			   ((= pb 1) (aref vec (+ i 2)))
			   (t (error "bad PB on LESS-THAN"))))
		      (c (cond ;; pb mode of 3rd parameter
			   ((= pa 0) (aref vec (aref vec (+ i 3))))
			   ((= pa 1) (aref vec (+ i 3)))
			   (t (error "bad PA on LESS-THAN")))))		      
		 (cond
		   ((< a b)
		    (format t "LESS THAN  ~a < ~a -> True writing 1 to ~a " a b c)
		    (setf (aref vec (aref vec (+ i 3))) 1)
		    (when (= pa 1) (error "PA=1 EQUALS")))
		   (t
		    (format t "LESS THAN  ~a < ~a -> False writing 0 to ~a " a b c)
		    (setf (aref vec (aref vec (+ i 3))) 0)
		    (when (= pa 1) (error "PA=1 EQUALS"))
		    ))
		 (setq i (+ i 4))))
	      

	      ((= op EQUALS) ;; OP A B C 
	       (let* ((a (cond ;; pc mode of 1st parameter
			   ((= pc 0) (aref vec (aref vec (+ i 1))))
			   ((= pc 1) (aref vec (+ i 1)))
			   (t (error "bad PC on EQUALS"))))
                      (b (cond ;; pb mode of 2nd parameter
			   ((= pb 0) (aref vec (aref vec (+ i 2))))
			   ((= pb 1) (aref vec (+ i 2)))
			   (t (error "bad PB on EQUALS"))))
		      (c (cond ;; pb mode of 3rd parameter
			   ((= pa 0) (aref vec (aref vec (+ i 3))))
			   ((= pa 1) (aref vec (+ i 3)))
			   (t (error "bad Pa on EQUALS")))))
		 (format t "POOR : PARAMS ~a ~a ~a ~%" a b c)
		 (cond
		   ((= a b)
		    (setf (aref vec (aref vec (+ i 3))) 1)
		    (format t "EQUALS -> TRUE . writing 1 to {~a} ~%" c)
		    (when (= pa 1) (error "PA=1 EQUALS"))
		    )
		   (t
		    (setf (aref vec (aref vec (+ i 3))) 0)
		    (format t "EQUALS -> FALSE . writing 0 to {~a} ~%" c)
		    (when (= pa 1) (error "PA=1 EQUALS"))
		    ))
		 (setq i (+ i 4))))

	      
	      
              ((= op ADD) ;; OP A B C
	       (let* ((a (cond ;; pc mode of 1st parameter
			   ((= pc 0) (aref vec (aref vec (+ i 1))))
			   ((= pc 1) (aref vec (+ i 1)))
			   (t (error "bad PC on ADD"))))
                      (b (cond ;; pb mode of 2nd parameter
			   ((= pb 0) (aref vec (aref vec (+ i 2))))
			   ((= pb 1) (aref vec (+ i 2)))
			   (t (error "bad PB on ADD"))))
                      (sum (+ a b)))
		 ;; implicit 3rd argument in code itself 
		 (when (not (zerop pa)) (error "bad PA on ADD"))
                 (setf (aref vec (aref vec (+ i 3))) sum)
		 (format t "ADD ~a + ~a -> ~a -> {~a}~%" a b sum (aref vec (+ i 3)))
                 (setq i (+ i 4))))
	      
              ((= op MULT) ;; OP A B C 
	       (let* ((a (cond ;; pc mode of 1st parameter
			   ((= pc 0) (aref vec (aref vec (+ i 1))))
			   ((= pc 1) (aref vec (+ i 1)))
			   (t (error "bad PC on MULT"))))
                      (b (cond ;; pb mode of 2nd parameter
			   ((= pb 0) (aref vec (aref vec (+ i 2))))
			   ((= pb 1) (aref vec (+ i 2)))
			   (t (error "bad PB on MULT"))))
                      (sum (* a b)))
		 (when (not (zerop pa)) (error "bad PA on MULT"))
                 (setf (aref vec (aref vec (+ i 3))) sum)
		 (format t "MULT ~a * ~a -> ~a -> {~a} ~%" a b sum (aref vec (+ i 3)))
                 (setq i (+ i 4))))
	      
              (t (error (format nil "run command OP ~a not understood" op)))))))))




;; make a copy of vector so as not to 
(defun vec-copy (v)
  (let ((arr (make-array (length v) :initial-contents v)))
    arr))




(defun part2()  (run (vec-copy (fresh-in)) (lambda () 5)))

(defun test-1()  (run (vec-copy (list 3 9 8 9 10 9 4 9 99 -1 8)) (lambda () 8)))
(defun test-2()  (run (vec-copy (list 3 9 8 9 10 9 4 9 99 -1 8)) (lambda () 3)))

;; test 3 , test 4 : is input < 8 
;; test-3 : 1
;; test-4 : 0 
(defun test-3()  (run (vec-copy (list 3 9 7 9 10 9 4 9 99 -1 8)) (lambda () 7)))
(defun test-4()  (run (vec-copy (list 3 9 7 9 10 9 4 9 99 -1 8)) (lambda () 8)))

;; test 5 , test 6 : is input = 8
;; test 5 : 1
;; test 6 : 0 
(defun test-5()  (run (vec-copy (list 3 3 1108 -1 8 3 4 3 99)) (lambda () 8)))
(defun test-6()  (run (vec-copy (list 3 3 1108 -1 8 3 4 3 99)) (lambda () 9)))

                                    
;; test 7 , test 8 : is input = 8
;; test 7 : 1
;; test 8 : 0 
(defun test-7()  (run (vec-copy (list 3 3 1107 -1 8 3 4 3 99)) (lambda () 6)))
(defun test-8()  (run (vec-copy (list 3 3 1107 -1 8 3 4 3 99)) (lambda () 9)))

;; test-9 , test-10 : output 0 if input 0 , otherwise output 1 if input nonzero
(defun test-9 () (run (vec-copy (list 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9))
		      (lambda () 0)))

(defun test-10 () (run (vec-copy (list 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9))
		      (lambda () 2)))



;; test-11 , test-12 , test-13 
;; The program will then output 999 if the input value is below 8, output
;; 1000 if the input value is equal to 8, or output 1001 if the input
;; value is greater than 8.

(defun test-11 () (run (vec-copy
			(list 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 
			      1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 
			      999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99))
		       (lambda () 6)))

(defun test-12 () (run (vec-copy
			(list 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 
			      1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 
			      999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99))
		       (lambda () 8)))

(defun test-13 () (run (vec-copy
			(list 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 
			      1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 
			      999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99))
		       (lambda () 10)))


#|
CL-USER> (aoc::part2)

POOR : PARAMS 0 5 
READ {0} <- 5 
CHECK at 225 is 5 

ADD 5 + 1100 -> 1105 -> {6}

JUMP-IF-TRUE activated to jumping to 238
JUMP-IF-TRUE skipped
JUMP-IF-TRUE activated to jumping to 247
JUMP-IF-TRUE skipped
JUMP-IF-TRUE activated to jumping to 256
JUMP-IF-FALSE skipped
JUMP-IF-FALSE activated to jumping to 265
JUMP-IF-FALSE skipped
JUMP-IF-FALSE activated to jumping to 274
JUMP-IF-TRUE activated to jumping to 280
ADD 5 + 5 -> 10 -> {225}

ADD 294 + 0 -> 294 -> {0}

JUMP-IF-TRUE activated to jumping to 294
JUMP-IF-FALSE activated to jumping to 300
ADD 10 + 10 -> 20 -> {225}

ADD 314 + 0 -> 314 -> {0}

JUMP-IF-FALSE activated to jumping to 314
POOR : PARAMS 226 677 0 
EQUALS -> FALSE . writing 0 to {0} 

MULT 0 * 2 -> 0 -> {223} 

JUMP-IF-FALSE activated to jumping to 329
POOR : PARAMS 226 677 0 
EQUALS -> FALSE . writing 0 to {0} 

MULT 2 * 0 -> 0 -> {223} 

JUMP-IF-FALSE activated to jumping to 344
LESS THAN  226 < 226 -> False writing 0 to 0 
MULT 0 * 2 -> 0 -> {223} 

JUMP-IF-FALSE activated to jumping to 359
POOR : PARAMS 677 226 0 
EQUALS -> FALSE . writing 0 to {0} 

MULT 2 * 0 -> 0 -> {223} 

JUMP-IF-TRUE skipped
ADD 0 + 1 -> 1 -> {223}

LESS THAN  677 < 226 -> False writing 0 to 0 
MULT 2 * 1 -> 2 -> {223} 

JUMP-IF-FALSE activated to jumping to 389
POOR : PARAMS 677 226 0 
EQUALS -> FALSE . writing 0 to {0} 

MULT 2 * 2 -> 4 -> {223} 

JUMP-IF-TRUE skipped
ADD 4 + 1 -> 5 -> {223}

LESS THAN  226 < 677 -> True writing 1 to 0 
MULT 2 * 5 -> 10 -> {223} 

JUMP-IF-TRUE activated to jumping to 419
POOR : PARAMS 677 226 1 
EQUALS -> FALSE . writing 0 to {1} 

MULT 2 * 10 -> 20 -> {223} 

JUMP-IF-TRUE skipped
ADD 1 + 20 -> 21 -> {223}

POOR : PARAMS 226 226 0 
EQUALS -> TRUE . writing 1 to {0} 

MULT 2 * 21 -> 42 -> {223} 

JUMP-IF-FALSE skipped
ADD 42 + 1 -> 43 -> {223}

LESS THAN  226 < 226 -> False writing 0 to 1 
MULT 2 * 43 -> 86 -> {223} 

JUMP-IF-FALSE activated to jumping to 464
POOR : PARAMS 677 677 0 
EQUALS -> TRUE . writing 1 to {0} 

MULT 86 * 2 -> 172 -> {223} 

JUMP-IF-TRUE activated to jumping to 479
LESS THAN  677 < 226 -> False writing 0 to 1 
MULT 2 * 172 -> 344 -> {223} 

JUMP-IF-FALSE activated to jumping to 494
LESS THAN  226 < 677 -> True writing 1 to 0 
MULT 344 * 2 -> 688 -> {223} 

JUMP-IF-TRUE activated to jumping to 509
LESS THAN  677 < 226 -> False writing 0 to 1 
MULT 2 * 688 -> 1376 -> {223} 

JUMP-IF-FALSE activated to jumping to 524
LESS THAN  226 < 226 -> False writing 0 to 0 
MULT 2 * 1376 -> 2752 -> {223} 

JUMP-IF-FALSE activated to jumping to 539
LESS THAN  226 < 677 -> True writing 1 to 0 
MULT 2752 * 2 -> 5504 -> {223} 

JUMP-IF-FALSE skipped
ADD 1 + 5504 -> 5505 -> {223}

POOR : PARAMS 677 226 1 
EQUALS -> FALSE . writing 0 to {1} 

MULT 2 * 5505 -> 11010 -> {223} 

JUMP-IF-FALSE activated to jumping to 569
LESS THAN  677 < 226 -> False writing 0 to 0 
MULT 11010 * 2 -> 22020 -> {223} 

JUMP-IF-TRUE skipped
ADD 1 + 22020 -> 22021 -> {223}

LESS THAN  226 < 677 -> True writing 1 to 0 
MULT 2 * 22021 -> 44042 -> {223} 

JUMP-IF-TRUE activated to jumping to 599
POOR : PARAMS 226 677 1 
EQUALS -> FALSE . writing 0 to {1} 

MULT 2 * 44042 -> 88084 -> {223} 

JUMP-IF-TRUE skipped
ADD 1 + 88084 -> 88085 -> {223}

POOR : PARAMS 677 677 0 
EQUALS -> TRUE . writing 1 to {0} 

MULT 2 * 88085 -> 176170 -> {223} 

JUMP-IF-TRUE activated to jumping to 629
LESS THAN  226 < 226 -> False writing 0 to 1 
MULT 2 * 176170 -> 352340 -> {223} 

JUMP-IF-FALSE activated to jumping to 644
POOR : PARAMS 226 226 0 
EQUALS -> TRUE . writing 1 to {0} 

MULT 352340 * 2 -> 704680 -> {223} 

JUMP-IF-FALSE skipped
ADD 1 + 704680 -> 704681 -> {223}

POOR : PARAMS 226 677 1 
EQUALS -> FALSE . writing 0 to {1} 

MULT 2 * 704681 -> 1409362 -> {223} 

JUMP-IF-TRUE skipped
ADD 1 + 1409362 -> 1409363 -> {223}

OUTPUT { 1409363 } 
POOR : PARAMS 1409363 
WRITE 1409363 

POOR : HALT !! 
T


............. 1409363  ........ should be result

|#
