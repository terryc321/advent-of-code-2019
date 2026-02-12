
#|
--- Day 4: Secure Container ---

You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

    It is a six-digit number.
    The value is within the range given in your puzzle input.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

Other than the range rule, the following are true:

    111111 meets these criteria (double 11, never decreases).
    223450 does not meet these criteria (decreasing pair of digits 50).
    123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input meet these criteria?

Your puzzle answer was 1748.

 Your puzzle input is still 146810-612564.
|#



(define knum
  (lambda (k)
    (define num2
      (lambda (n from to k2)
	;; what if n not in range from to ?
	(cond
	 ((and (>= n from)(<= n to))
	  (k2 n)
	  (num2 (+ n 1) from to k2))
	 (#t #f))))
    (num2 0 0 9 k)))
	


(define brute
  (lambda ()
  ;; for a from 1 to 6
  ;;  for b from 1 to 4
  ;;   for c from 2 to
    (knum (lambda (a) ;; 1
	    (knum (lambda (b) ;; 2
		    (knum (lambda (c) ;; 3
			    (knum (lambda (d) ;; 4
				    (knum (lambda (e) ;; 5
					    (knum (lambda (f) ;; 6th
						    (let ((val (+ (* f (expt 10 0))
								  (* e (expt 10 1))
								  (* d (expt 10 2))
								  (* c (expt 10 3))
								  (* b (expt 10 4))
								  (* a (expt 10 5)))))
						      (format #t "n = ~a ~a ~a ~a ~a ~a : ~a ~%" a b c d e f val))))))))))))))))


;; six counters initial value 146810 
(define a 1)
(define b 4)
(define c 6)
(define d 8)
(define e 1)
(define f 0)
(define show-ct 0)

(define (reset)
  (set! a 1)
  (set! b 4)
  (set! c 6)
  (set! d 8)
  (set! e 1)
  (set! f -1)
  (set! show-ct 0)
  )



(define (increment)
  (set! f (+ f 1))
  (when (> f 9)
    (set! f 0)
    (set! e (+ e 1))
    (when (> e 9)
      (set! e 0)
      (set! d (+ d 1))
      (when (> d 9)
	(set! d 0)
	(set! c (+ c 1))
	(when (> c 9)
	  (set! c 0)
	  (set! b (+ b 1))
	  (when (> b 9)
	    (set! b 0)
	    (set! a (+ a 1))
	    (when (> a 9)
	      #f)))))))

(define show
  (lambda ()
    (let ((val (+ (* f (expt 10 0))
		  (* e (expt 10 1))
		  (* d (expt 10 2))
		  (* c (expt 10 3))
		  (* b (expt 10 4))
		  (* a (expt 10 5)))))
      ;; #t
      (set! show-ct (+ show-ct 1))
      (format #t "n = ~a ~a ~a ~a ~a ~a : ~a : ~a~%" a b c d e f val show-ct)
      )))

(define (end-condition)
  (and (= a 6)(= b 1)(= c 2)(= d 5)(= e 6)(= f (+ 4 1))))

(define (step)
  ;; if acceptable just show + increment / else just increment
  (cond
   ((and (<= a b)(<= b c)(<= c d)(<= d e)(<= e f))  (show) (increment))
   (#t (increment)))  
  (cond
   ((end-condition) #f)
   (#t (step))))


(define (run)
  (reset)
  (step))

  

