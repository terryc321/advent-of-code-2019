
;; chicken scheme
(import (chicken format))


(define (input)
  (list->vector (list 1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 10 23 2 10 23 27 1 27 6 31 1 13 31 35 1 13 35 39 1 39 10 43 2 43 13 47 1 47 9 51 2 51 13 55 1 5 55 59 2 59 9 63 1 13 63 67 2 13 67 71 1 71 5 75 2 75 13 79 1 79 6 83 1 83 5 87 2 87 6 91 1 5 91 95 1 95 13 99 2 99 6 103 1 5 103 107 1 107 9 111 2 6 111 115 1 5 115 119 1 119 2 123 1 6 123 0 99 2 14 0 0)))

;; (define (mk-state v i)
;;   (vector 'state v i))

;; (define (state? s)
;;   (and (vector? s)(eq? (vector-ref s 0) 'state)))

;; (define (state-vector s)
;;   (cond
;;    ((state? s) (vector-ref s 1))
;;    (#t (error "not a state"))))

;; (define (state-index s)
;;   (cond
;;    ((state? s) (vector-ref s 2))
;;    (#t (error "not a state"))))

;; (define (state-read s i)
;;   (cond
;;    ((state? s) (vector-ref (state-vector s) i))
;;    (#t (error "not a state"))))

;; (define (state-write s i n)
;;   (cond
;;    ((state? s) (let* ((v1 (state-vector s))
;; 		      (vlen (vector-length v1))
;; 		      (v2 (make-vector vlen)))
;; 		 (vector-copy! v1 v2)
;; 		 (vector-set! v2 i n)
;; 		 (mk-state v2 i)))
;;    (#t (error "not a state"))))

;; (define (initial-state)
;;   (mk-state (input) 0))

(define verbose #f)

;; copy by coercion
(define (vector-copy v)
  (list->vector (vector->list v)))



(define (sim v i)
  (let ((op (vector-ref v i)))
    (cond
     ((= op 1) (sim-add v i))
     ((= op 2) (sim-mul v i))
     ((= op 3) (sim-store v i))     
     ((= op 99)
      (when verbose (format #t "sim-halted ~a ~%" v))
      v)
     (#t (error "bad state")))))


(define (sim-add v i)
  (let* ((i1 (vector-ref v (+ i 1)))
	 (i2 (vector-ref v (+ i 2)))
	 (n1 (vector-ref v i1))
	 (n2 (vector-ref v i2))
	 (dest (vector-ref v (+ i 3)))	 
	 (r1 (+ n1 n2))
	 (v2 (vector-copy v)))    
    (when verbose
      (format #t "sim-add ~a ~a at ~a => ~a ~%" n1 n2 dest r1))
    (vector-set! v2 dest r1)
    (sim v2 (+ i 4))))


(define (sim-mul v i)
  (let* ((i1 (vector-ref v (+ i 1)))
	 (i2 (vector-ref v (+ i 2)))
	 (n1 (vector-ref v i1))
	 (n2 (vector-ref v i2))
	 (dest (vector-ref v (+ i 3)))	 
	 (r1 (* n1 n2))
	 (v2 (vector-copy v)))    
    (when verbose
      (format #t "sim-mul ~a ~a at ~a => ~a ~%" n1 n2 dest r1))
    (vector-set! v2 dest r1)
    (sim v2 (+ i 4))))


;; using read -- but where does read get its input from ?
(define (sim-store v i)
  (let* ((dest (vector-ref v (+ i 1)))	 
	 (v2 (vector-copy v)))    
    (when verbose
      (format #t "sim-store ~a => ~a ~%" dest))
    (vector-set! v2 dest (read))
    (sim v2 (+ i 2))))


;; 1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
;; 2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
;; 2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
;; 1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.

(define (example-trial xs)  (sim (list->vector xs) 0))
(define (ex1)  (example-trial '(1 0 0 0 99)))
(define (ex2)  (example-trial '(2 3 0 3 99)))
(define (ex3)  (example-trial '(2 4 4 5 99 0)))
(define (ex4)  (example-trial '(1 1 1 4 99 5 6 0 99)))

;; replace 1 with 12
;; replace 2 with 2
(define (day2-part1)
  (let ((in (input)))
    (vector-set! in 1 12)
    (vector-set! in 2 2)
    (sim in 0)))


(define (day2-part2)
  (let ((target 19690720))
    (letrec ((rec (lambda (n v)
		    (cond
		     ((> v 99) #f)
		     ((> n 99) (rec 0 (+ v 1)))
		     (#t (let ((in (input)))
			   (vector-set! in 1 n)
			   (vector-set! in 2 v)
			   (let ((out (sim in 0)))
			     (cond
			      ((= (vector-ref out 0) target)
			       (let ((nv100 (+ (* 100 n) v)))
				 (format #t "n ~a : v ~a : 100n+v = ~a ~%" n v nv100)))
			      (#t (rec (+ n 1) v))))))))))
      (rec 0 0))))

;; gets a little bit involved now...


