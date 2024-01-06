

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
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

;;(chdir "day1")

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

#|

some test cases

    For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
    For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
    For a mass of 1969, the fuel required is 654.
    For a mass of 100756, the fuel required is 33583.


    A module of mass 14 requires 2 fuel. This fuel requires no further
fuel (2 divided by 3 and rounded down is 0, which would call for a
negative fuel), so the total fuel required is still just 2.  At first,
a module of mass 1969 requires 654 fuel. Then, this fuel requires 216
more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which
requires 21 fuel, which requires 5 fuel, which requires no further
fuel. So, the total fuel required for a module of mass 1969 is 654 +
216 + 70 + 21 + 5 = 966.  The fuel required by a module of mass 100756
and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2
= 50346.


|#

;; recursive fuel
(define (fuel m)
  (-  (floor (/ m 3)) 2))

(define (fuel3 m tot)
  (let ((r (fuel m)))
    (cond
     ((> r 0) (fuel3 r (+ tot r)))
     (#t tot))))

(define (fuel2 m)
  (let ((tot 0))
    (fuel3 m tot)))




(define (test)
  (map fuel2 '(12 14 1969 100756)))

(define (f2)
  (apply +  (map fuel2 input)))

#|
scheme@(guile-user)> (f1)
$9 = 3497399

scheme@(guile-user)> (f2)
$12 = 5243207


|#



