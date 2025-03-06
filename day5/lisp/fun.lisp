

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

(defun computer-input ()
  1)

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

(defun run (vec)
  (setq *vector* vec)
  (let ((i 0)(ins nil)(HALT 99)(ADD 1)(MULT 2)(READ 3)(WRITE 4))
    (catch 'done
      (loop while t do
        (setq ins (aref vec i))
	(multiple-value-bind (op pa pb pc) (config ins)
	      
            (cond
              ((= op HALT) (throw 'done t))

	      ((= op WRITE) (let ((a (cond ;; pc mode of 1st parameter
				       ((= pc 0) (aref vec (aref vec (+ i 1))))
				       ((= pc 1) (aref vec (+ i 1)))
				       (t (error "bad PC on WRITE")))))
			      (computer-output a)
			      (format t "WRITE ~a ~%" a)
                              (setq i (+ i 2))))
              
	      ((= op READ) (let* ((a (cond ;; pc mode of 1st parameter
				       ((= pc 0) (aref vec (aref vec (+ i 1))))
				       ((= pc 1) (aref vec (+ i 1)))
				       (t (error "bad PC on READ"))))
                                  (b (computer-input)))
                             (setf (aref vec (aref vec (+ i 1))) b)
			     (format t "READ ~a <- ~a ~%" a b)
			     (format t "CHECK at ~a is ~a "
				     (aref vec (+ i 1))
				     (aref vec (aref vec (+ i 1))))
                             (setq i (+ i 2))))
              
              ((= op ADD) (let* ((a (cond ;; pc mode of 1st parameter
				      ((= pc 0) (aref vec (aref vec (+ i 1))))
				      ((= pc 1) (aref vec (+ i 1)))
				       (t (error "bad PC on ADD"))))
                                 (b (cond ;; pb mode of 2nd parameter
				      ((= pb 0) (aref vec (aref vec (+ i 2))))
				      ((= pb 1) (aref vec (+ i 2)))
				       (t (error "bad PB on ADD"))))
                                 (sum (+ a b)))
			    (when (not (zerop pa)) (error "bad PA on ADD"))
                            (setf (aref vec (aref vec (+ i 3))) sum)
			    (format t "ADD ~a + ~a -> ~a -> {~a}~%" a b sum (aref vec (+ i 3)))
                            (setq i (+ i 4))))
	      
              ((= op MULT) (let* ((a (cond ;; pc mode of 1st parameter
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




(defun part1()
  (run (vec-copy (fresh-in))))


#|
READ 0 <- 1 
CHECK at 225 is 1 ADD 1 + 1100 -> 1101 -> {6}
ADD 1 + 238 -> 239 -> {225}
OUTPUT { 0 } 
WRITE 0 
ADD 13 + 55 -> 68 -> {224}
ADD 68 + -68 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 0 * 8 -> 0 -> {223} 
ADD 0 + 4 -> 4 -> {224}
ADD 4 + 0 -> 4 -> {223}
ADD 62 + 41 -> 103 -> {225}
ADD 83 + 71 -> 154 -> {225}
MULT 59 * 16 -> 944 -> {224} 
ADD -944 + 944 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 4 * 8 -> 32 -> {223} 
ADD 3 + 0 -> 3 -> {224}
ADD 3 + 32 -> 35 -> {223}
MULT 71 * 55 -> 3905 -> {224} 
ADD 3905 + -3905 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 35 * 8 -> 280 -> {223} 
ADD 7 + 0 -> 7 -> {224}
ADD 280 + 7 -> 287 -> {223}
ADD 6 + 94 -> 100 -> {224}
ADD -100 + 100 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 287 * 8 -> 2296 -> {223} 
ADD 6 + 0 -> 6 -> {224}
ADD 6 + 2296 -> 2302 -> {223}
MULT 75 * 30 -> 2250 -> {225} 
MULT 70 * 44 -> 3080 -> {224} 
ADD -3080 + 3080 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 2302 * 8 -> 18416 -> {223} 
ADD 0 + 4 -> 4 -> {224}
ADD 18416 + 4 -> 18420 -> {223}
ADD 55 + 20 -> 75 -> {225}
MULT 55 * 16 -> 880 -> {225} 
MULT 13 * 94 -> 1222 -> {225} 
MULT 16 * 55 -> 880 -> {225} 
MULT 13 * 13 -> 169 -> {225} 
ADD 75 + 13 -> 88 -> {224}
ADD -88 + 88 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 18420 * 8 -> 147360 -> {223} 
ADD 0 + 2 -> 2 -> {224}
ADD 147360 + 2 -> 147362 -> {223}
MULT 20 * 57 -> 1140 -> {224} 
ADD -1140 + 1140 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 147362 * 8 -> 1178896 -> {223} 
ADD 6 + 0 -> 6 -> {224}
ADD 1178896 + 6 -> 1178902 -> {223}
ADD 76 + 62 -> 138 -> {224}
ADD 138 + -138 -> 0 -> {224}
OUTPUT { 0 } 
WRITE 0 
MULT 1178902 * 8 -> 9431216 -> {223} 
ADD 5 + 0 -> 5 -> {224}
ADD 9431216 + 5 -> 9431221 -> {223}
OUTPUT { 9431221 } 
WRITE 9431221 
T

CODE should be 9431221




|#























