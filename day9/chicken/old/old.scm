;; chicken specific stuff
(import (chicken format))
(format #t "hello world~%")

(import srfi-1)

(define (input)
  '(3 8 1001 8 10 8 105 1 0 0 21 46 67 76 97 118 199 280 361 442 99999 3 9 1002 9 3 9 101 4 9 9 102 3 9 9 1001 9 3 9 1002 9 2 9 4 9 99 3 9 102 2 9 9 101 5 9 9 1002 9 2 9 101 2 9 9 4 9 99 3 9 101 4 9 9 4 9 99 3 9 1001 9 4 9 102 2 9 9 1001 9 4 9 1002 9 5 9 4 9 99 3 9 102 3 9 9 1001 9 2 9 1002 9 3 9 1001 9 3 9 4 9 99 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 99 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 99))

;; % becomes modulo
;; (modulo 123 100) = 23

;; parameter op 
(define (pmop n k)
  (k (modulo n 100)))

;; parameter-mode-1
(define (pm1 n k)
  (cond
   ((< n 100) (k 0))
   (else (k (floor (/ (modulo n 1000) 100))))))

;; parameter-mode-2
(define (pm2 n k)
  (cond
   ((< n 1000) (k 0))
   (else (k (floor (/ (modulo n 10000) 1000))))))

;; parameter-mode-3
(define (pm3 n k)
  (cond
   ((< n 10000) (k 0))
   (else (k (floor (/ (modulo n 100000) 10000))))))

;; permutation 0 1 2 3 4 ( 5 items in total 0 - 4 inclusive)
;; day7

;; cps loop while ip >= 0 ip < length vector 
;; opcode is 99 then halt
;; opcode is 1 then add
;; opcode is 2 then multiply

(define *write* #f) 
(define *read* #f)  

;; some defaults , writer dumps to stdout , reader takes 1 always ??
(set! *write* 
  (lambda (n) 
    (format #t "(DEFAULT_WROTE ~a)" n)))
(set! *read* 
      (lambda () 
	(format #t "(DEFAULT_READ 1)~%")
	1))

;; routine can take any number of arguments
(define kerr (lambda args (format #t "kerror ~a~%" args)))

(define *step* 0)

;; here is our tape interpreter
(define (interpret tape ip kok kerr)  
  (set! *step* (+ *step* 1))
  (format #t "step ~a : tape ~a : ip ~a~%" *step* tape ip)
  (cond
   ((< ip 0) (kerr "ip < 0" ip))
   ((>= ip (vector-length tape)) (kerr "ip > tape size"))
   (else (pmop (vector-ref tape ip)
	       (lambda (op)
		 (cond
		  ((= op 99)(khalt tape ip kok kerr))
		  ((= op 1) (kadd tape ip kok kerr))
		  ((= op 2) (kmult tape ip kok kerr))
		  ((= op 3) (kread tape ip kok kerr))
		  ((= op 4) (kwrite tape ip kok kerr))
		  ((= op 5) (kjmp_true tape ip kok kerr)) 
		  ((= op 6) (kjmp_false tape ip kok kerr)) 
		  ((= op 7) (kless_than tape ip kok kerr)) 
		  ((= op 8) (kequals tape ip kok kerr)) ;; TODO
		  (else (kerr "bad opcode"))))))))


(define (kequals tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))

   ;; arg2 -- offset ip + 2
   (cond
     ((= p2 0) "position mode"
      (set! arg2 (vector-ref tape (+ ip 2)))
      (set! arg2 (vector-ref tape arg2)))
     (else "immediate mode"
	   (set! arg2 (vector-ref tape (+ ip 2)))))

   ;; if the first parameter is less than the second parameter, it stores 1 in the position 
   ;; given by the third parameter. Otherwise, it stores 0.
   (cond 
    ((= arg1 arg2) (vector-set! tape arg3 1))     
    (else (vector-set! tape arg3 0)))
   (interpret tape (+ ip 4) kok kerr)))))))))


(define (kless_than tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))

   ;; arg2 -- offset ip + 2
   (cond
     ((= p2 0) "position mode"
      (set! arg2 (vector-ref tape (+ ip 2)))
      (set! arg2 (vector-ref tape arg2)))
     (else "immediate mode"
	   (set! arg2 (vector-ref tape (+ ip 2)))))

   ;; if the first parameter is less than the second parameter, it stores 1 in the position 
   ;; given by the third parameter. Otherwise, it stores 0.
   (cond 
    ((< arg1 arg2) (vector-set! tape arg3 1))     
    (else (vector-set! tape arg3 0)))
   (interpret tape (+ ip 4) kok kerr)))))))))



(define (kjmp_false tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))

   ;; arg2 -- offset ip + 2
   (cond
     ((= p2 0) "position mode"
      (set! arg2 (vector-ref tape (+ ip 2)))
      (set! arg2 (vector-ref tape arg2)))
     (else "immediate mode"
	   (set! arg2 (vector-ref tape (+ ip 2)))))

;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.

   (cond 
    ((= arg1 0) (let ((ip2 arg2)) 
		  (interpret tape ip2 kok kerr)))
    (else 
     (interpret tape (+ ip 3) kok kerr)))))))))))





(define (kjmp_true tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))

   ;; arg2 -- offset ip + 2
   (cond
     ((= p2 0) "position mode"
      (set! arg2 (vector-ref tape (+ ip 2)))
      (set! arg2 (vector-ref tape arg2)))
     (else "immediate mode"
	   (set! arg2 (vector-ref tape (+ ip 2)))))

;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.   
   (cond 
    ((not (= arg1 0)) (let ((ip2 arg2)) 
			(interpret tape ip2 kok kerr)))
    (else 
     (interpret tape (+ ip 3) kok kerr)))))))))))


(define (kread tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
  (set! p1 1) ;; use immediate mode for writing to destinations
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))

   ;; p2 not actually used ?
   ;; p3 not actually used ?
   (format #t "reading ~a into position ~a of tape ~a ~%" (*read*) arg1 (vector-length tape))
   (vector-set! tape arg1 (*read*))
   (interpret tape (+ ip 2) kok kerr)))))))))

(define (kwrite tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
   (set! p1 1) ;; force immediate mode ?
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))
   ;; p2 not actually used ?
   ;; p3 not actually used ?
   (*write* arg1)
   (interpret tape (+ ip 2) kok kerr)))))))))
				     



(define (kadd tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))

   ;; arg2 -- offset ip + 2
   (cond
     ((= p2 0) "position mode"
      (set! arg2 (vector-ref tape (+ ip 2)))
      (set! arg2 (vector-ref tape arg2)))
     (else "immediate mode"
	   (set! arg2 (vector-ref tape (+ ip 2)))))
   ;; p3 not actually used ?
   (set! arg3 (vector-ref tape (+ ip 3)))
   (let ((result (+ arg1 arg2)))
     (vector-set! tape arg3 result)
     (interpret tape (+ ip 4) kok kerr))))))))))
				     

(define (kmult tape ip kok kerr)
  (let* ((pms (vector-ref tape ip))
	 (arg1 0)
	 (arg2 0)
	 (arg3 0))
    (pm1 pms (lambda (p1)
      (pm2 pms (lambda (p2)
	(pm3 pms (lambda (p3)

  ;; arg1 -- offset ip + 1
   (cond
     ((= p1 0) "position mode"
      (set! arg1 (vector-ref tape (+ ip 1)))
      (set! arg1 (vector-ref tape arg1)))
     (else "immediate mode"
	   (set! arg1 (vector-ref tape (+ ip 1)))))

   ;; arg2 -- offset ip + 2
   (cond
     ((= p2 0) "position mode"
      (set! arg2 (vector-ref tape (+ ip 2)))
      (set! arg2 (vector-ref tape arg2)))
     (else "immediate mode"
	   (set! arg2 (vector-ref tape (+ ip 2)))))
   ;; p3 not actually used ?
   (set! arg3 (vector-ref tape (+ ip 3)))
   (let ((result (* arg1 arg2)))
     (vector-set! tape arg3 result)
     (interpret tape (+ ip 4) kok kerr))))))))))


(define (khalt tape ip kok kerr)
  ;;(format #t "halt!~a~%~a~%" tape ip)
  (kok tape ip))

(define kok (lambda args
  (format #t "OK!~%~a~%" args)))


(define (mktape xs)
  (list->vector xs))

;; ;; some tests
;; (1 0 0 0 99) becomes 2 0 0 0 99 (1 + 1 = 2).
;; (2 3 0 3 99) becomes 2 3 0 6 99 (3 * 2 = 6).
;; (2 4 4 5 99 0) becomes 2 4 4 5 99 9801 (99 * 99 = 9801).
;; (1 1 1 4 99 5 6 0 99) becomes 30 1 1 4 2 5 6 0 99.

(define (test1)
  (let ((ip 0))
    (interpret (mktape '(1 0 0 0 99)) ip kok kerr)))

(define (test2)
  (let ((ip 0))
    (interpret (mktape '(2 3 0 3 99)) ip kok kerr)))

(define (test3)
  (let ((ip 0))
    (interpret (mktape '(2 4 4 5 99 0)) ip kok kerr)))

(define (test4)
  (let ((ip 0))
    (interpret (mktape '(1 1 1 4 99 5 6 0 99)) ip kok kerr)))

(define (day2-tape) 
  (mktape '(1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 10 23 2 10 23 27 1 27 6 31 1 13 31 35 1 13 35 39 1 39 10 43 2 43 13 47 1 47 9 51 2 51 13 55 1 5 55 59 2 59 9 63 1 13 63 67 2 13 67 71 1 71 5 75 2 75 13 79 1 79 6 83 1 83 5 87 2 87 6 91 1 5 91 95 1 95 13 99 2 99 6 103 1 5 103 107 1 107 9 111 2 6 111 115 1 5 115 119 1 119 2 123 1 6 123 0 99 2 14 0 0 )))

(define (test5) ;; day2 part1
 (let ((ip 0))
   (interpret (day2-tape) ip kok kerr)))    


(define (test5b) ;; can we get output on tape[0] ?
 (let ((ip 0)
       (kok (lambda args 
	      (let* ((tape (first args))
		     (output (vector-ref tape 0)))
		output))))
   (interpret (day2-tape) ip kok kerr)))    


;; greater or equal continuation
(define (kge a b ktrue kfalse)
  (if (>= a b) (ktrue 0) (kfalse 0)))

(define (kle a b ktrue kfalse)
  (if (<= a b) (ktrue 0) (kfalse 0)))

;; lets see a looping cps 
;; loop between 0 and 99 inclusive
;; TEST -> is i between 0 and 99 ? YES -> kyes 
;; NO -> kno 
;; (define (loop99 i kyes kno)
;;   (cond
;;    ((and (>= i 0)(<= i 99)) 
;;      (kyes i (lambda () (loop99 (+ i 1) kyes kno))))
;;    (else kno #f)))
;;   (loop99 0 (lambda (a f)
;; 	      (loop99 0 (lambda (b g)
;; 			  (format #t "a = ~a :  b = ~a ~%" a b)
;; 			  (g))
;; 		      (lambda (d) d)))
;; 	  (lambda (d) d)))


;; a ,b indices incremented during loop99 recursion
;; f takes a b kyes kno 
;;   if f(a,b) solves problem - kyes is called , if not kno called
(define (loop99 a b fn kyes kno)
  (cond
   ((> a 99) (loop99 0 (+ b 1) fn kyes kno))
   ((> b 99) (kno))
   (else (fn a b kyes (lambda ()
		       (loop99 (+ a 1) b fn kyes kno))))))

(define (test6)
  (let ((kyes (lambda (noun verb)
		(let ((prod (+ (* 100 noun) verb)))
		  (format #t "found solution : noun ~a : verb ~a : prod ~a ~%"
			  noun verb prod))))
	(kno (lambda ()(format #t "no solutions found ~%")))
	(fn (lambda (a b kyes kno) 
	      ;; make a bespoke machine 
	      ;; with noun as tape[1] , verb as tape[2] 
	      (let* ((ip 0)
		     (kok (lambda args 
			    (let* ((tape (first args))
				   (output (vector-ref tape 0)))
			      output)))
		     (tape (let ((t (day2-tape)))
			     (vector-set! t 1 a)
			     (vector-set! t 2 b)
			     t))
		     (output (interpret tape ip kok kerr)))	      
	      (cond 
	       ((= output 19690720) (kyes a b))
	       (#t (kno)))))))
    (loop99 0 0 fn kyes kno)))



;; day5 examples 
;; 3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, 
;; consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
(define (test7)
  (let ((ip 0)
	(tape-list '(3 9 8 9 10 9 4 9 99 -1 8))
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  
    (format #t "setting read to always give 1~%")
    (set! *read* (lambda () 1))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)

    (format #t "setting read to always give 8~%")
    (set! *read* (lambda () 8))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)
))


;; 3,9,7,9,10,9,4,9,99,-1,8 - 
;; Using position mode, consider whether the input is less than 8; 
;; output 1 (if it is) or 0 (if it is not).
(define (test8)
  (let ((ip 0)
	(tape-list '(3 9 7 9 10 9 4 9 99 -1 8))
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  
    (format #t "setting read to always give 1~%")
    (set! *read* (lambda () 1))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)

    (format #t "setting read to always give 8~%")
    (set! *read* (lambda () 12))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)
))



;; 3,3,1108,-1,8,3,4,3,99 - Using immediate mode, 
;; consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
(define (test9)
  (let ((ip 0)
	(tape-list '(3 3 1108 -1 8 3 4 3 99))
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  
    (format #t "setting read to always give 1~%")
    (set! *read* (lambda () 1))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)

    (format #t "setting read to always give 8~%")
    (set! *read* (lambda () 8))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)
))

;; 3,3,1107,-1,8,3,4,3,99 - Using immediate mode, 
;; consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
(define (test10)
  (let ((ip 0)
	(tape-list '(3 3 1107 -1 8 3 4 3 99))
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  
    (format #t "setting read to always give 1~%")
    (set! *read* (lambda () 1))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)

    (format #t "setting read to always give 8~%")
    (set! *read* (lambda () 8))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)
))


;; Here are some jump tests that take an input, 
;; then output 0 if the input was zero or 1 if the input was non-zero:
;; 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (using position mode)
(define (test11)
  (let ((ip 0)
	(tape-list '(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9))
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  
    (set! *step* 0)
    (format #t "setting read to always give 1~%")
    (set! *read* (lambda () 1))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)

    (set! *step* 0)
    (format #t "setting read to always give 0~%")
    (set! *read* (lambda () 0))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)
))


;; 3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (using immediate mode)
;; then output 0 if the input was zero or 1 if the input was non-zero:
(define (test12)
  (let ((ip 0)
	(tape-list '(3 3 1105 -1 9 1101 0 0 12 4 12 99 1))
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  
    (format #t "setting read to always give 1~%")
    (set! *read* (lambda () 1))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)

    (format #t "setting read to always give 8~%")
    (set! *read* (lambda () 0))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (mktape tape-list) ip kok kerr)
))


;; (test11) are supposed to return 0 or 1 ?? we get 3 ??
;; (test12) are supposed to return 0 or 1 ??

(define (day5-tape)
  (mktape '(
3 225 1 225 6 6 1100 1 238 225 104 0 1001 152 55 224 1001 224 -68 224 4 224 1002 223 8 223 1001 224 4 224 1 224 223 223 1101 62 41 225 1101 83 71 225 102 59 147 224 101 -944 224 224 4 224 1002 223 8 223 101 3 224 224 1 224 223 223 2 40 139 224 1001 224 -3905 224 4 224 1002 223 8 223 101 7 224 224 1 223 224 223 1101 6 94 224 101 -100 224 224 4 224 1002 223 8 223 101 6 224 224 1 224 223 223 1102 75 30 225 1102 70 44 224 101 -3080 224 224 4 224 1002 223 8 223 1001 224 4 224 1 223 224 223 1101 55 20 225 1102 55 16 225 1102 13 94 225 1102 16 55 225 1102 13 13 225 1 109 143 224 101 -88 224 224 4 224 1002 223 8 223 1001 224 2 224 1 223 224 223 1002 136 57 224 101 -1140 224 224 4 224 1002 223 8 223 101 6 224 224 1 223 224 223 101 76 35 224 1001 224 -138 224 4 224 1002 223 8 223 101 5 224 224 1 223 224 223 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 1008 677 677 224 1002 223 2 223 1006 224 329 1001 223 1 223 8 677 226 224 102 2 223 223 1006 224 344 101 1 223 223 1107 226 226 224 1002 223 2 223 1006 224 359 1001 223 1 223 1108 677 226 224 102 2 223 223 1005 224 374 1001 223 1 223 1007 226 226 224 102 2 223 223 1006 224 389 1001 223 1 223 108 677 677 224 1002 223 2 223 1005 224 404 1001 223 1 223 1007 677 677 224 102 2 223 223 1005 224 419 1001 223 1 223 8 226 677 224 102 2 223 223 1005 224 434 101 1 223 223 1008 677 226 224 102 2 223 223 1006 224 449 1001 223 1 223 7 677 677 224 102 2 223 223 1006 224 464 1001 223 1 223 8 226 226 224 1002 223 2 223 1005 224 479 1001 223 1 223 7 226 677 224 102 2 223 223 1006 224 494 1001 223 1 223 7 677 226 224 1002 223 2 223 1005 224 509 101 1 223 223 107 677 677 224 102 2 223 223 1006 224 524 101 1 223 223 1007 677 226 224 102 2 223 223 1006 224 539 101 1 223 223 107 226 226 224 1002 223 2 223 1006 224 554 101 1 223 223 1008 226 226 224 102 2 223 223 1006 224 569 1001 223 1 223 1107 677 226 224 1002 223 2 223 1005 224 584 101 1 223 223 1107 226 677 224 102 2 223 223 1005 224 599 101 1 223 223 1108 226 677 224 102 2 223 223 1005 224 614 101 1 223 223 108 677 226 224 102 2 223 223 1005 224 629 101 1 223 223 107 226 677 224 102 2 223 223 1006 224 644 1001 223 1 223 1108 226 226 224 1002 223 2 223 1006 224 659 101 1 223 223 108 226 226 224 102 2 223 223 1005 224 674 101 1 223 223 4 223 99 226
)))


(define (day5-part1)
  (let ((ip 0)
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  

    (format #t "setting read to always give 1~%")
    (set! *read* (lambda () 1))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (day5-tape) ip kok kerr)
))

(define (day5-part2)
  (let ((ip 0)
	(kok (lambda args 
	       (let* ((tape (first args))
		      (output (vector-ref tape 0)))
		 (format #t "output was ~a~%" output)
		 output))))		  

    (format #t "setting read to always give 5~%")
    (set! *read* (lambda () 5))
    (set! *write* (lambda (n) (format #t "(WROTE ~a) ~%" n)))
    (interpret (day5-tape) ip kok kerr)
))
