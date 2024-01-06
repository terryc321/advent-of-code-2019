

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...

;; regular expression
(use-modules (ice-9 regex)) 

;; --------------------- macros --------------------------
;; can we not do this using hygienic macros ...

(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			    ,@body
			    (,fn (cdr ,xs))))))))
       (,fn ,ls))))

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

;;(chdir "day2")

(define *debug* #f)

(define input #f)

(define (get-input filename)
  (let ((port (open-input-file filename)))
    (set! input (read port))
    (close-input-port port)))

;; for example
(define input #f)

(get-input "input")
;;(get-input "input2")

;; array zero based processing
(set! input (list->vector input))


#|

opcodee
1 add
2 mul
99 halt


|#


(define (process v)
  (cond
   ((list? v) (process (list->vector v)))
   ((vector? v) 
    (let ((pos 0))
      (process2 v pos)))
   (#t (error "process" (list "expected a list or vector" v)))))



(define (process2 v i)
  ;;(format #t "~a : ~%~a ~%" v i) 
  (let ((op (vector-ref v i)))
    (cond
     ((= op 99) 
      ;;(format #t "halt op ~%")
      ;;(values 'done v)
      v
      )
     ((= op 1) ;;do add
      ;;(format #t "add op ~%")
      (let ((n (+ (vector-ref v (vector-ref v (+ i 1)))
		  (vector-ref v (vector-ref v (+ i 2))))))
	(vector-set! v (vector-ref v (+ i 3)) n)
	(process2 v (+ i 4))))
     ((= op 2) ;;do mul
      ;;(format #t "multiply op ~%")
      (let ((n (* (vector-ref v (vector-ref v (+ i 1)))
		  (vector-ref v (vector-ref v (+ i 2))))))
	(vector-set! v (vector-ref v (+ i 3)) n)
	(process2 v (+ i 4))))
     (#t
      (error "process2" (list "op not recognised" op))))))






(define (example)
  (process (list->vector '(1 9 10 3 2 3 11 0 99 30 40 50))))

#|
    1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
    2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
    2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
    1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.
  |# 

(define (check)
  (assert (equal? (process '(1 0 0 0 99)) #(2 0 0 0 99)))
  (assert (equal? (process '(2 3 0 3 99 )) #(2 3 0 6 99 )))
  (assert (equal? (process '(2 4 4 5 99 0 )) #(2 4 4 5 99 9801 )))
  (assert (equal? (process '(1 1 1 4 99 5 6 0 99 )) #(30 1 1 4 2 5 6 0 99 ))))


;; replace input
;; 1 with 12
;; 2 with 2

(define (f1)
  (get-input "input")
  (set! input (list->vector input))
  (vector-set! input 1 12)
  (vector-set! input 2 2)  
  (process input)
  )


#|
iterative search 
noun 0 .. 99
verb 0 .. 99

reset
 loads input from file stores list in tmp variable-bound?
 whenever reset is called generates a fresh vector from this stored tmp variable-bound?
 only ever reads file once

no for loop in guile scheme ?


|#
(define reset
  (let ((tmp 0))
    (get-input "input")
    (set! tmp input)
    (lambda () ;; procedure here ...
      (set! input (list->vector tmp))
      input)))
  

(define (f2 a b)
  (cond
   ((> a 99) (f2 0 (+ b 1)))
   ((> b 99) 'not-found)
   (#t 
    (reset)
    (vector-set! input 1 a)
    (vector-set! input 2 b)
    (process input)
    (when (= (vector-ref input 0) 19690720)
      (format #t "solution ~a ~a : ~a~%" a b (+ (* 100 a) b)))
    (f2 (+ a 1) b))))
   

(define (f3)
  (f2 0 0))





#|
------ results ----------
(f1)
$30 = #(4090701 12 2 2 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 60 1 19 10 64 2
10 23 256 1 27 6 258 1 13 31 263 1 13 35 268 1 39 10 272 2 43 13 1360
1 47 9 1363 2 51 13 6815 1 5 55 6816 2 59 9 20448 1 13 63 20453 2 13
67 102265 1 71 5 102266 2 75 13 511330 1 79 6 511332 1 83 5 511333 2
87 6 1022666 1 5 91 1022667 1 95 13 1022672 2 99 6 2045344 1 5 103
2045345 1 107 9 2045348 2 6 111 4090696 1 5 115 4090697 1 119 2
4090699 1 6 123 0 99 2 14 0 0)

4090701

scheme@(guile-user)> (f3)
solution 64 21 : 6421
$33 = not-found

6421

accepted answer


|#



