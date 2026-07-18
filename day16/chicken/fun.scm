
(import srfi-1)
(import (chicken format))

(define (puzzle-input)
  59791871295565763701016897619826042828489762561088671462844257824181773959378451545496856546977738269316476252007337723213764111739273853838263490797537518598068506295920453784323102711076199873965167380615581655722603274071905196479183784242751952907811639233611953974790911995969892452680719302157414006993581489851373437232026983879051072177169134936382717591977532100847960279215345839529957631823999672462823375150436036034669895698554251454360619461187935247975515899240563842707592332912229870540467459067349550810656761293464130493621641378182308112022182608407992098591711589507803865093164025433086372658152474941776320203179747991102193608
  )

;; base pattern
(define (base) '(0 1 0 -1))

(define (make-base-fn base repeat)
  (let ((ptr base)
	(rep repeat))
    (lambda (op)
      (cond
       ((equal? op 'next)	
	(let ((res (car ptr)))
	  (set! rep (- rep 1))
	  (when (= rep 0)
	    (set! ptr (cdr ptr))
	    (set! rep repeat))	  
	  (when (null? ptr)
	    (set! ptr base))
	  res))
       (else (error "make-base-fn unknown cmd"))))))

;;(let ((bf (make-base-fn '(1 2 3) 1)))  (map (lambda (n) (bf 'next)) (iota 12)))
;;(let ((bf (make-base-fn '(1 2 3) 2)))  (map (lambda (n) (bf 'next)) (iota 12)))
;;(let ((bf (make-base-fn '(1 2 3) 3)))  (map (lambda (n) (bf 'next)) (iota 12)))
;;(let ((bf (make-base-fn '(1 2 3) 4)))  (map (lambda (n) (bf 'next)) (iota 12)))
;;(let ((bf (make-base-fn '(1 2 3) 5)))  (map (lambda (n) (bf 'next)) (iota 12)))
;;(let ((bf (make-base-fn '(1 2 3) 10)))  (map (lambda (n) (bf 'next)) (iota 21)))

(let ((bf (make-base-fn '(0 1 0 -1) 1)))  (map (lambda (n) (bf 'next)) (iota 21)))
(let ((bf (make-base-fn '(0 1 0 -1) 2)))  (map (lambda (n) (bf 'next)) (iota 21)))
(let ((bf (make-base-fn '(0 1 0 -1) 3)))  (map (lambda (n) (bf 'next)) (iota 21)))
(let ((bf (make-base-fn '(0 1 0 -1) 4)))  (map (lambda (n) (bf 'next)) (iota 21)))
(let ((bf (make-base-fn '(0 1 0 -1) 12345)))  (map (lambda (n) (bf 'next)) (iota 12)))


;; for a given pattern 9 8 7 6 5 
;; 
(define (make-patt-fn patt repeat)
  (let ((ptr patt)
	(rep repeat)
	(done #f))
    (lambda (op)
      (cond
       ((equal? op 'reset) ;; reset-able
	(set! ptr patt)
	(set! rep repeat)
	(set! done #f)
	'reset)
       ((equal? op 'next) ;; iterator
	(cond
	 (done 'done)
	 (else
	  (when (null? ptr)
	    (set! ptr patt)
	    (set! rep (- rep 1)))
	  (when (< rep 1)
	    (set! done #t)
	    'done)
	  (cond
	   (done 'done)
	   (else 
	    (let ((res (car ptr)))
	      (set! ptr (cdr ptr))
	      res))))))
	(else (error "make-patt-fn unknown cmd"))))))

;;
(define (ex1-helper n)  
  (let ((pf (make-patt-fn '(1 2 3 4 5 6 7 8) 1)))
    (let* ((bf (make-base-fn '(0 1 0 -1) n)))
      (pf 'reset)
      (bf 'next) ;; we discard first base value
      (let ((sum 0))
	(letrec ((exhaust
		  (lambda ()
		    (let ((p (pf 'next))
			  (b (bf 'next)))
		      (cond
		       ((eq? p 'done)
			;;(format #t "sum is ~a ~%" sum)
			(modulo (abs sum) 10))
		       (#t (let ((mul (* p b)))
			     (set! sum (+ sum mul))
			     ;;(format #t "~a * ~a => ~a : ~a~%" p b (* p b) sum)
			     (exhaust))))))))
	    ;;(format #t "==============~%")
  	    (exhaust))))))

(define (ex1)
  (let ((res '()))
    (format #t "~%answer =>~%")
    (let loop ((i 1))
      (let ((v (ex1-helper i)))
	(format #t "~a" v)
	(cond
	 ((< i 8) (loop (+ i 1))))))
    (format #t "~%")))

;; ==========================================================

(define (bigex1-helper patt ex n)  
  (let ((pf (make-patt-fn patt ex)))
    (let* ((bf (make-base-fn '(0 1 0 -1) n)))
      (pf 'reset)
      (bf 'next) ;; we discard first base value
      (let ((sum 0))
	(letrec ((exhaust
		  (lambda ()
		    (let ((p (pf 'next))
			  (b (bf 'next)))
		      (cond
		       ((eq? p 'done)
			;;(format #t "sum is ~a ~%" sum)
			(modulo (abs sum) 10))
		       (#t (let ((mul (* p b)))
			     (set! sum (+ sum mul))
			     ;;(format #t "~a * ~a => ~a : ~a~%" p b (* p b) sum)
			     (exhaust))))))))
	    ;;(format #t "==============~%")
  	    (exhaust))))))

(define (bigex1)
  (let* ((res '())
	 (ex 10000) ;;10000) ;; ten-thousand times
	 (patt '(1 2 3 4 5 6 7 8));;(explode (pattern-input)))
	 (plim (* ex (length patt))))
    (format #t "~%answer =>~%")
    (let loop ((i 1))
      (let ((v (bigex1-helper patt ex i)))
	(format #t "~a" v)
	(cond
	 ((< i plim) (loop (+ i 1))))))
    (format #t "~%")))

(bigex1)



#|

  (ex1-helper 1)
  (ex1-helper 2))



1*0  + 2*1  + 3*1  + 4*0  + 5*0  + 6*-1 + 7*-1 + 8*0  = 8

#;1239> (+ 2 5 5 5 -2 -10)
5

#;1257> (ex1)
1 * 1 => 1 : 1
2 * 0 => 0 : 1
3 * -1 => -3 : -2
4 * 0 => 0 : -2
6 * 1 => 6 : 4
7 * 0 => 0 : 4
8 * -1 => -8 : -4
sum is -4 
==============
1 * 0 => 0 : 0
2 * 1 => 2 : 2
3 * 1 => 3 : 5
4 * 0 => 0 : 5
6 * 0 => 0 : 5
7 * -1 => -7 : -2
8 * -1 => -8 : -10
sum is -10 
0
#;1258> (+ 2 3 -7 -8)
-10
#;1264> 
|#
;; keep hammering on m until it returns 'done 
(define (exhaust m i)
  (let ((v (m 'next)))
    (cond
     ((eq? v 'done) v)
     (else
      ;; (when (zero? (modulo i 100000))
      ;; 	(format #t "v(~a) = ~a~%" i v))
      (exhaust m (+ i 1))))))

;;(define pf (make-patt-fn '(9 8 7 6 5) 100))
(define (demo)
  (let ((pf (make-patt-fn '(9 8 7 6 5) 100)))
    (exhaust pf 0)))

(define (demo2)
  (let ((pf (make-patt-fn (explode (puzzle-input)) 10000)))
    (exhaust pf 0)))


;; (let ((pf (make-patt-fn '(9 8 7 6 5) 1)))
;;   (let loop ()
;;     (let ((v (pf 'next)))
;;       (cond
;;        ((equal? v #f) 'done)
;;        (else (format #t "~a " v)
;; 	     (loop))))))


(define (explode n)
  (map (lambda (ch)
	 (case ch
	   ((#\0) 0)
	   ((#\1) 1)
	   ((#\2) 2)
	   ((#\3) 3)
	   ((#\4) 4)
	   ((#\5) 5)
	   ((#\6) 6)
	   ((#\7) 7)
	   ((#\8) 8)
	   ((#\9) 9)))
       (string->list (format #f "~a" n))))

(explode 123)



;; =============================================


(define (repeat-seq e n)
  (cond
   ((< n 1) '())
   (#t (cons e (repeat-seq e (- n 1))))))

(define (repeat-base? xs n)
  (cond
   ((null? xs) '())
   (#t (append (repeat-seq (car xs) n)
	       (repeat-base? (cdr xs) n)))))

(define (extend-base rs n)
  (let ((rlen (length rs)))
    (cond
     ((> rlen n) rs)
     (#t (extend-base (append rs rs) n)))))

(define (row base pat dim)
  (cdr (extend-base (repeat-base? base dim) (length pat))))

(define (signed-modulo x y)
  (assert (positive? y))
  (cond
   ((negative? x) (- (modulo (- x) y)))
   (#t (modulo x y))))

(signed-modulo -3 10)

(define (mulr is ps)
  (define (recur is2 ps2)
    (cond
     ((null? is2) '())
     ((null? ps2) (recur is2 ps))
     (#t (cons (signed-modulo (* (car is2) (car ps2)) 10)
	       (recur (cdr is2) (cdr ps2))))))
  (recur is ps))


(let ((base '(0 1 0 -1))
      (pat '(1 2 3 4 5 6 7 8)))
  (list (modulo (abs (apply + (mulr pat (row base pat 1)))) 10)
	(modulo (abs (apply + (mulr pat (row base pat 2)))) 10)
	(modulo (abs (apply + (mulr pat (row base pat 3)))) 10)
	(modulo (abs (apply + (mulr pat (row base pat 4)))) 10)
	(modulo (abs (apply + (mulr pat (row base pat 5)))) 10)
	(modulo (abs (apply + (mulr pat (row base pat 6)))) 10)
	(modulo (abs (apply + (mulr pat (row base pat 7)))) 10)
	(modulo (abs (apply + (mulr pat (row base pat 8)))) 10)))

(let* ((base '(0 1 0 -1))
       (pat '(1 2 3 4 5 6 7 8))
       (rows (cdr (iota (+ 1 (length pat))))))
  (map (lambda (d)
	 (modulo (abs (apply + (mulr pat (row base pat d)))) 10))
       rows))


(define (pass #!key base pat)
  (let ((rows (cdr (iota (+ 1 (length pat))))))
    (let ((fn (lambda (d) (modulo (abs (apply + (mulr pat (row base pat d)))) 10))))
      (map fn rows))))

;; ;; specialise base
;; (define spec-base
;;   (lambda (#!key base)
;;     (lambda (#!key pat)
;;       (pass base: base pat: pat))))
;; (define fv (spec-base base:'(0 1 0 -1)))

(define (run #!key base pat lim)
  (let* ((out pat))
    (let loop ((count 1))
      (set! pat out)
      (set! out (pass pat: pat base: base))    
      (cond
       ((< count lim) (loop (+ count 1)))
       (#t out)))))

(run base: '(0 1 0 -1) pat: '(1 2 3 4 5 6 7 8) lim: 1)
(run base: '(0 1 0 -1) pat: '(1 2 3 4 5 6 7 8) lim: 2)
(run base: '(0 1 0 -1) pat: '(1 2 3 4 5 6 7 8) lim: 3)
(run base: '(0 1 0 -1) pat: '(1 2 3 4 5 6 7 8) lim: 4)

(case #\b
  ((#\a) 1)
  ((#\b) 2))


(take (run base: '(0 1 0 -1) pat: (explode 80871224585914546619083218645595) lim: 100) 8)
;; (2 4 1 7 6 1 7 6)
(take (run base: '(0 1 0 -1) pat: (explode 19617804207202209144916044189917 ) lim: 100) 8)
;; (7 3 7 4 5 4 1 8)
(take (run base: '(0 1 0 -1) pat: (explode 69317163492948606335995924319873 ) lim: 100) 8)
;; (5 2 4 3 2 1 3 3)

(string-length "59791871295565763701016897619826042828489762561088671462844257824181773959378451545496856546977738269316476252007337723213764111739273853838263490797537518598068506295920453784323102711076199873965167380615581655722603274071905196479183784242751952907811639233611953974790911995969892452680719302157414006993581489851373437232026983879051072177169134936382717591977532100847960279215345839529957631823999672462823375150436036034669895698554251454360619461187935247975515899240563842707592332912229870540467459067349550810656761293464130493621641378182308112022182608407992098591711589507803865093164025433086372658152474941776320203179747991102193608")

(* 650 10000)
;; array of size 6.5 million ?
;;6_500_000

(define (part1)
  (let ((in 59791871295565763701016897619826042828489762561088671462844257824181773959378451545496856546977738269316476252007337723213764111739273853838263490797537518598068506295920453784323102711076199873965167380615581655722603274071905196479183784242751952907811639233611953974790911995969892452680719302157414006993581489851373437232026983879051072177169134936382717591977532100847960279215345839529957631823999672462823375150436036034669895698554251454360619461187935247975515899240563842707592332912229870540467459067349550810656761293464130493621641378182308112022182608407992098591711589507803865093164025433086372658152474941776320203179747991102193608))
    (take (run base: '(0 1 0 -1) pat: (explode in) lim: 100) 8)))


(define (repeat-input in n)
  (cond
   ((< n 2) in)
   (else 
    (let ((res in))
      (let loop ((count 2))
	(set! res (append in res))
	(cond
	 ((>= count n) res)
	 (else (loop (+ count 1)))))))))

;; (repeat-input '(1 2 3) 4)

(define (part2)
  (let* ((in 59791871295565763701016897619826042828489762561088671462844257824181773959378451545496856546977738269316476252007337723213764111739273853838263490797537518598068506295920453784323102711076199873965167380615581655722603274071905196479183784242751952907811639233611953974790911995969892452680719302157414006993581489851373437232026983879051072177169134936382717591977532100847960279215345839529957631823999672462823375150436036034669895698554251454360619461187935247975515899240563842707592332912229870540467459067349550810656761293464130493621641378182308112022182608407992098591711589507803865093164025433086372658152474941776320203179747991102193608)
	 (bigin (repeat-input (explode in) 10000)))
    (take (run base: '(0 1 0 -1) pat: bigin lim: 100) 8)))


;;(format #t "solution ~a~%" (part1))
;;(format #t "solution ~a~%" (part2))
    
#|

 (main) ~/code/advent-code/advent-of-code-2019/day16/chicken$ time ./fun
solution (5 9 5 2 2 4 2 2)

Error: (apply) stack overflow

	Call history:

	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  
	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  
	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  
	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  
	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  
	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  
	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  
	fun.scm:41: signed-modulo	  
	fun.scm:42: recur	  	<--

real	0m8.429s
user	0m7.675s
sys	0m0.753s

stack overflow huh 

|#

;;   (fv pat))
;;   (let ((r1 (pass pat: pat base: base)))
;;     (pass pat: r1 base: base)))



;; (let ((base '(0 1 0 -1))
;;       (pat '(1 2 3 4 5 6 7 8)))
;;   (let ((r1 (pass pat: pat base: base)))
;;     (pass pat: r1 base: base)))

;; Input signal: 12345678

;; 1*1  + 2*0  + 3*-1 + 4*0  + 5*1  + 6*0  + 7*-1 + 8*0  = 4
;; 1*0  + 2*1  + 3*1  + 4*0  + 5*0  + 6*-1 + 7*-1 + 8*0  = 8
;; 1*0  + 2*0  + 3*1  + 4*1  + 5*1  + 6*0  + 7*0  + 8*0  = 2
;; 1*0  + 2*0  + 3*0  + 4*1  + 5*1  + 6*1  + 7*1  + 8*0  = 2
;; 1*0  + 2*0  + 3*0  + 4*0  + 5*1  + 6*1  + 7*1  + 8*1  = 6
;; 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*1  + 7*1  + 8*1  = 1
;; 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*1  + 8*1  = 5
;; 1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*0  + 8*1  = 8

;; (+ 1 -3 5 -7)



;; (extendo-patt '(1 2 3) 4)

;; (define (repeat-base #!key pat base n)
;;   (let ((ex (extendo-patt (repeat-base? base n) n)))
;;     (assert (> (length ex) (length pat)))
;;     (cdr ex)))


;; (let ((pat '(1 2 3 4 5 6 7 8))
;;       (base '(0 1 0 -1)))
;;   (mulr pat (repeat-base base: base pat: pat n: 1)))

;; (mulr '(1 2 3 4 5 6 7 8) (repeat-base (base) 1))
;; (repeat-base (base) 2)
;; (repeat-base (base) 3)
;; (repeat-base (base) 4)
;; (repeat-base (base) 5)




