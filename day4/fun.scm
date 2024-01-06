

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...

;; regular expression
(use-modules (ice-9 regex)) 

(use-modules (ice-9 match)) 

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

;;(chdir "day3")
;; get current working directory
;;  (getcwd)


(define *debug* #f)

(define input #f)

(define (get-input filename)
  (let ((port (open-input-file filename)))
    (set! input (read port))
    (close-input-port port)))

;; for example
(define input #f)

;;(get-input "input")
;;(get-input "input2")

(set! input 146810-612564)

#|
 six digit number 
 left to right numbers never decrease
 increase or stay same
 two adjacent digits are the same

146810
612564
abcdef
|#

#|
(dolist (a (iota 10))
	(dolist (b (iota 10))
		
		(dolist (c (iota 10))
			(dolist (d (iota 10))
				(dolist (e (iota 10))
					(dolist (f (iota 10))
|#						

(define (inrange a b c d e f)
  (let ((k (+ (* f 1)
	      (* e 10)
	      (* d 100)
	      (* c 1000)
	      (* b 10000)
	      (* a 100000))))
    (and (>= k 146810) ;; low 
	 (<= k 612564) ;; high
	 (>= b a) ;; well ordered
	 (>= c b)
	 (>= d c)
	 (>= e d)
	 (>= f e)
	 (or (= a b) ;; pair 
	     (= b c)
	     (= c d)
	     (= d e)
	     (= e f)))))



(define (foo)
  (let ((a 0)(b 0)(c 0)(d 0)(e 0)(f 0)(step 0))

    (while (< a 10)
      (set! b a)
      (while (< b 10)
	(set! c b)
	(while (< c 10)
	  (set! d c)
	  (while (< d 10)
	    (set! e d)
	    (while (< e 10)
	      (set! f e)
	      (while (< f 10)

		(cond 
		 ((inrange a b c d e f)
		  (set! step (+ 1 step))
		  (format #t "~a : ~a ~a ~a ~a ~a ~a ~%" step a b c d e f)
		  )
		 (#t 
		  ;;(format #t "NOP : ~a ~a ~a ~a ~a ~a ~%"  a b c d e f))
		  #f
		 ))

		(set! f (+ f 1)))
	      (set! e (+ e 1)))
	    (set! d (+ d 1)))
	  (set! c (+ c 1)))
	(set! b (+ b 1)))
      (set! a (+ a 1)))))

#|
running (foo)
displays 1748 different passwords


not really understood part 2 of the puzzle 

series of 111 's does not contain a pair of 11 s ??
i dont get it


|#








    


      
    
    
  






































