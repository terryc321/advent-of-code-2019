
;; stklos 
(require-feature 'srfi-1)
;; format predefined

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
	 (line 0)
	 (col 0)
	 ;; (buf (make-vector 80 #f))
	 (ex 10000) ;;10000) ;; ten-thousand times
	 (patt '(1 2 3 4 5 6 7 8));;(explode (pattern-input)))
	 (plim (* ex (length patt))))
    (format #t "~%answer =>~%")
    (let loop ((i 1))
      (let ((v (bigex1-helper patt ex i)))
	(format #t "~a" v)
	;;(vector-set! buf col v)
	(set! col (+ col 1))
	(when (= col 80)	  
	  (set! col 0)
	  (format #t "~%")
	  ;; (set! line (+ line 1))
	  ;; (format #t "line ~a / 81250 : ~a~%" line buf))
	  )
	(cond
	 ((< i plim) (loop (+ i 1))))))
    (format #t "~%")))

;;(bigex1)

;;(floor (/ 6500000 80)) if we print number 6.5 million digits with 80 digits
;;81250 lines long 

;; (compile-file "fun.stklos.scm" "fun.stklos.so")
;; (load "fun.stklos.so")
;; (bigex1)
