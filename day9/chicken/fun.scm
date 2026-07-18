;; -*- geiser-scheme-implementation: chicken -*-

#|

now int code uses arbitrarily large numbers and arbitrarily large memory

if we have a hash table for memory we can instead of vector-ref just return 0 if
its out of normal size tape

removed phase table - phases no longer a thing


|#

  


(import (chicken format))
(import (chicken process-context)) ;; change-directory current-directory
(import procedural-macros)
(import srfi-1)
(import srfi-69) ;; hash-tables
;; native support for records 
(import srfi-9) 
(import test) ;; testing framework
(import (chicken base)) ;; do i need to do this ?
(import srfi-39) ;; dynamic variables using parameterise 
(import bindings) ;; (bind (a b c) '(1 2 3) ...
(import test) ;; simple testing framework

(define day9input '(1102 34463338 34463338 63 1007 63 34463338 63 1005 63 53 1101 3 0 1000 109 988 209 12 9 1000 209 6 209 3 203 0 1008 1000 1 63 1005 63 65 1008 1000 2 63 1005 63 904 1008 1000 0 63 1005 63 58 4 25 104 0 99 4 0 104 0 99 4 17 104 0 99 0 0 1102 1 31 1008 1101 682 0 1027 1101 0 844 1029 1102 29 1 1001 1102 1 22 1014 1101 0 21 1011 1102 428 1 1025 1101 0 433 1024 1101 0 38 1019 1102 1 37 1016 1102 35 1 1017 1102 39 1 1018 1102 32 1 1000 1102 23 1 1012 1102 1 329 1022 1102 26 1 1006 1102 1 24 1003 1102 28 1 1005 1102 36 1 1010 1102 34 1 1004 1101 0 1 1021 1102 326 1 1023 1101 33 0 1015 1101 20 0 1002 1101 0 25 1007 1101 0 853 1028 1102 27 1 1009 1102 1 30 1013 1101 689 0 1026 1102 1 0 1020 109 12 2108 30 -3 63 1005 63 201 1001 64 1 64 1105 1 203 4 187 1002 64 2 64 109 -9 2101 0 6 63 1008 63 29 63 1005 63 227 1001 64 1 64 1106 0 229 4 209 1002 64 2 64 109 -6 1208 5 22 63 1005 63 249 1001 64 1 64 1106 0 251 4 235 1002 64 2 64 109 13 21107 40 41 8 1005 1018 273 4 257 1001 64 1 64 1105 1 273 1002 64 2 64 109 -11 2102 1 8 63 1008 63 25 63 1005 63 299 4 279 1001 64 1 64 1105 1 299 1002 64 2 64 109 15 1205 7 317 4 305 1001 64 1 64 1105 1 317 1002 64 2 64 109 10 2105 1 -1 1105 1 335 4 323 1001 64 1 64 1002 64 2 64 109 -22 1202 1 1 63 1008 63 24 63 1005 63 357 4 341 1106 0 361 1001 64 1 64 1002 64 2 64 109 13 1206 6 373 1106 0 379 4 367 1001 64 1 64 1002 64 2 64 109 11 1206 -6 393 4 385 1105 1 397 1001 64 1 64 1002 64 2 64 109 -32 1208 10 34 63 1005 63 419 4 403 1001 64 1 64 1105 1 419 1002 64 2 64 109 30 2105 1 0 4 425 1106 0 437 1001 64 1 64 1002 64 2 64 109 -28 1207 6 21 63 1005 63 455 4 443 1106 0 459 1001 64 1 64 1002 64 2 64 109 4 2101 0 8 63 1008 63 31 63 1005 63 485 4 465 1001 64 1 64 1105 1 485 1002 64 2 64 109 5 1207 -4 28 63 1005 63 505 1001 64 1 64 1106 0 507 4 491 1002 64 2 64 109 9 21102 41 1 2 1008 1016 39 63 1005 63 531 1001 64 1 64 1106 0 533 4 513 1002 64 2 64 109 -10 1201 4 0 63 1008 63 30 63 1005 63 553 1106 0 559 4 539 1001 64 1 64 1002 64 2 64 109 19 21108 42 41 -4 1005 1019 579 1001 64 1 64 1106 0 581 4 565 1002 64 2 64 109 -26 1201 3 0 63 1008 63 32 63 1005 63 607 4 587 1001 64 1 64 1106 0 607 1002 64 2 64 109 20 1205 3 623 1001 64 1 64 1105 1 625 4 613 1002 64 2 64 109 2 21107 43 42 -1 1005 1018 645 1001 64 1 64 1106 0 647 4 631 1002 64 2 64 109 -11 2102 1 1 63 1008 63 29 63 1005 63 667 1105 1 673 4 653 1001 64 1 64 1002 64 2 64 109 27 2106 0 -8 1001 64 1 64 1105 1 691 4 679 1002 64 2 64 109 -25 2107 25 -4 63 1005 63 713 4 697 1001 64 1 64 1105 1 713 1002 64 2 64 109 -2 21108 44 44 2 1005 1010 735 4 719 1001 64 1 64 1106 0 735 1002 64 2 64 109 11 21101 45 0 -3 1008 1016 45 63 1005 63 757 4 741 1106 0 761 1001 64 1 64 1002 64 2 64 109 -15 1202 3 1 63 1008 63 22 63 1005 63 781 1105 1 787 4 767 1001 64 1 64 1002 64 2 64 109 6 21101 46 0 0 1008 1010 49 63 1005 63 811 1001 64 1 64 1105 1 813 4 793 1002 64 2 64 109 -7 2108 34 1 63 1005 63 835 4 819 1001 64 1 64 1105 1 835 1002 64 2 64 109 15 2106 0 10 4 841 1001 64 1 64 1106 0 853 1002 64 2 64 109 -25 2107 33 7 63 1005 63 873 1001 64 1 64 1106 0 875 4 859 1002 64 2 64 109 7 21102 47 1 10 1008 1010 47 63 1005 63 897 4 881 1105 1 901 1001 64 1 64 4 64 99 21102 1 27 1 21102 915 1 0 1105 1 922 21201 1 12038 1 204 1 99 109 3 1207 -2 3 63 1005 63 964 21201 -2 -1 1 21102 942 1 0 1105 1 922 21202 1 1 -1 21201 -2 -3 1 21101 0 957 0 1106 0 922 22201 1 -1 -2 1106 0 968 22101 0 -2 -2 109 -3 2105 1 0 ))

;; how do i add docs to chicken scheme procedure ?
;; mktape creates a hash table memory
(define (mktape xs)
  (assert (list? xs))
  (let ((hash (make-hash-table test: = )))
    (letrec ((iter (lambda (xs i)
		     (cond
		      ((null? xs) #t)
		      (else
		       (hash-table-set! hash i (car xs))
		       (iter (cdr xs) (+ i 1)))))))
      (iter xs 0)
      hash)))


;; lookup memory address i 
(define (tape-lookup h i)
  (assert (>= i 0))
  (let ((default-value 0))
    (hash-table-ref/default h i default-value)))

(define (tape-write! h i v)
  (assert (>= i 0))
  (hash-table-set! h i v))


(define *verbose* (make-parameter #t))


;; halt-table #f means not yet halted
(define *halt-table* (make-vector 5 #f))

;; initial phases - this is set by permutation 
(define *phase-table* (make-vector 5 #f))

;; mail-table #f means no mail for that respective machine yet
;; a machine has to wait to write into the mailbox 
(define *mailbox-table* (make-vector 5 #f))


(define-syntax %param
  (syntax-rules ()
    ((_ tape p arg *rel*) (cond
		     ((= p 0) (tape-lookup tape arg))
		     ((= p 1) arg)
		     ((= p 2) (tape-lookup tape (+ arg *rel*)))
		     (else (error "%param mode expected 0 1 2"))))))

;; we can use the function as a pure procedure jump
;;        (lambda (tape ip reader writer ident)
(define-syntax binary-operation
  (syntax-rules ()
    ((_ name op tape ip *rel*)
     (define (name)
	 (let* ((arg1 (tape-lookup tape (+ ip 1)))
		(arg2 (tape-lookup tape (+ ip 2)))
		(arg3 (tape-lookup tape (+ ip 3)))
		(p1 (param-1 (tape-lookup tape ip)))
		(p2 (param-2 (tape-lookup tape ip)))
		;; p3 not required as its writing	  
		(v1 (%param tape p1 arg1 *rel*))
		(v2 (%param tape p2 arg2 *rel*))
		(v3 arg3)
		)
	   (tape-write! tape v3 (op v1 v2))
	   (set! ip (+ ip 4)))))))

(define-syntax conditional-binary-operation
  (syntax-rules ()
    ((_ name op tape ip *rel*)
     (define (name)
	 (let* ((arg1 (tape-lookup tape (+ ip 1)))
		(arg2 (tape-lookup tape (+ ip 2)))
		(arg3 (tape-lookup tape (+ ip 3)))
		(p1 (param-1 (tape-lookup tape ip)))
		(p2 (param-2 (tape-lookup tape ip)))

		(v1 (%param tape p1 arg1 *rel*)) 
		(v2 (%param tape p2 arg2 *rel*)) 
		(v3 arg3)
		)
	   ;; condition
	   (cond
	    ((op v1 v2) (tape-write! tape v3 1))
	    (else (tape-write! tape v3 0)))
	   ;; next
	   (set! ip (+ ip 4)))))))

(define-syntax conditional-jump
  (syntax-rules ()
    ((_ name op tape ip *rel*)
     (define (name)
       (let* ((arg1 (tape-lookup tape (+ ip 1)))
	      (arg2 (tape-lookup tape (+ ip 2)))
	      (p1 (param-1 (tape-lookup tape ip)))
	      (p2 (param-2 (tape-lookup tape ip)))	   
	      (v1 (%param tape p1 arg1 *rel*)) ;;(if (zero? p1) (tape-lookup tape arg1) arg1))
	      (v2 (%param tape p2 arg2 *rel*)) ;;(if (zero? p2) (tape-lookup tape arg2) arg2))
	      )
	 (cond
	  ((op v1)
	   ;; set ip to 2nd parameter
	   (set! ip v2))
	  (else
	   ;; skip over
	   (set! ip (+ ip 3)))))))))


;; parameter modes 
(define param-op
  (lambda (n)
    (modulo n 100)))

(define param-1
  (lambda (n)
    (cond
     ((< n 100) 0)
     (else (floor (/ (modulo n 1000) 100))))))

(define param-2
  (lambda (n)
    (cond
     ((< n 1000) 0)
     (else (floor (/ (modulo n 10000) 1000))))))

(define param-3
  (lambda (n)
    (cond
     ((< n 10000) 0)
     (else (floor (/ (modulo n 100000) 10000))))))

(define (op-to-string op)
  (cond
   ((= op 1) "add")
   ((= op 2) "multiply")
   ((= op 3) "read")
   ((= op 4) "write")
   ((= op 5) "jump_if_true")
   ((= op 6) "jump_if_false")
   ((= op 7) "less_than")
   ((= op 8) "equals")
   ((= op 9) "adjust_relative_base")   
   ((= op 99) "halt")   
   (else (format #f "unknown-~a" op))))



#|
machine 
|#
(define (make-machine #!key (ident 0) (tapelist '()) (relbase 0))
  (let ((ip 0)
	(*relative-base* relbase) ;; start with zero relative base 
	(tape (mktape tapelist))
	(status 'inactive)
	(halted #f))
    
    (binary-operation i_add + tape ip *relative-base*)
    (binary-operation i_multiply * tape ip *relative-base*)
    (conditional-binary-operation i_less_than < tape ip *relative-base*)     
    (conditional-binary-operation i_equals = tape ip *relative-base*)
    (conditional-jump i_jump_true (lambda (v) (not (zero? v))) tape ip *relative-base*)
    (conditional-jump i_jump_false zero? tape ip *relative-base*)


    (define i_adjust_relative_base
      (lambda ()
	 (let* ((arg1 (tape-lookup tape (+ ip 1)))
		(p1 (param-1 (tape-lookup tape ip)))
		(v1 (%param tape p1 arg1 *relative-base*))) ;; arg1 ?
	   (set! *relative-base* (+ *relative-base* v1))
	   (format #t "relative-base = ~a~%" *relative-base*)
	   (set! ip (+ ip 2)))))

    
    ;; reading phase will always succeed - its there at outset (hopefully..)
    ;;
    ;; (cond
    ;; 	   ((< reads 1)
    ;; 	    (set! reads (+ 1 reads))
    ;; 	    (set! in (vector-ref *phase-table* ident))
    ;; 	    (let* ((arg1 (tape-lookup tape (+ ip 1)))
    ;; 		   (p1 (param-1 (tape-lookup tape ip)))
    ;; 		   (v1 (%param tape p1 arg1 *relative-base*))) ;; arg1 
    ;; 	      (tape-write! tape v1 in)
    ;; 	      (set! ip (+ ip 2))))
    ;; 	   ((equal? #f (vector-ref *mailbox-table* ident)) (set! status 'block-read))
    ;; 	   (else ;; assume its a number
	    
    ;; more complex local state - first read comes from phase table 
    (define i_read 
      (let ((reads 0))
	(lambda ()
	  (let* ((arg1 (tape-lookup tape (+ ip 1)))
		   (p1 (param-1 (tape-lookup tape ip)))
		   (v1 arg1) ;;(%param tape p1 arg1 *relative-base*)) ;;arg1)
		   (in (vector-ref *mailbox-table* ident)))
	      (set! status 'working)
	      ;; reset inbox to empty as taken a value
	      (vector-set! *mailbox-table* ident #f)
	      ;; change tape and continue 
	      (when (*verbose*)  (format #t "read in ~a ~%" in))      
	      (tape-write! tape v1 in)
	      (set! ip (+ ip 2))))))

    ;;  0 1 2 3 4
    (define i_write 
      (lambda ()
	(let ((destination (+ ident 1))
	      (to-thrusters #f))
	  ;; (when (= destination 5)
	  ;;   (set! to-thrusters #t) 
	  ;;   (set! destination 0))
	(cond
	 ((integer? (vector-ref *mailbox-table* destination))
	  (set! status 'block-write))
	 (else ;; assume destination mailbox empty
	  (let* ((arg1 (tape-lookup tape (+ ip 1)))
		 (p1 (param-1 (tape-lookup tape ip)))
		 (v1 (%param tape p1 arg1 *relative-base*)))
	    (vector-set! *mailbox-table* destination v1)
	    ;; thunderbirds are go !!
	    ;; (when to-thrusters (set! *thrusters* v1))
	    (set! ip (+ ip 2))))))))

    
    
    (define (i_halt)
      (set! halted #t)
      (vector-set! *halt-table* ident #t))
    (define (i_step)
      (cond
       (halted #f)
       (else 
	(let* ((raw (tape-lookup tape ip))
	       (op (param-op raw)))
	  (when (*verbose*)
	    (format #t "step (ip)~a : (raw) ~a : (op)~a:~a ~%" ip raw op (op-to-string op)))
	  (cond
	   ((= op 1)  (i_add))  
	   ((= op 2)  (i_multiply)) 
	   ((= op 3)  (i_read))  ;;
	   ((= op 4)  (i_write)) ;;
	   ((= op 5)  (i_jump_true)) 
	   ((= op 6)  (i_jump_false))
	   ((= op 7)  (i_less_than)) 
	   ((= op 8)  (i_equals))
	   ((= op 9)  (i_adjust_relative_base))
	   ((= op 99) (i_halt))     
	   (else (error "unknown opcode")))))))
    (lambda (cmd . args)
      (cond
       ((equal? cmd 'ip) ip)
       ((equal? cmd 'step) (i_step))
       ((equal? cmd 'tape) tape)
       ((equal? cmd 'halted?) halted)
       ((equal? cmd 'relbase) *relative-base*)
       ((equal? cmd 'status) status)))))



;; removed phases entirely
(define (run #!key (input 0) (tapelist '()) (relbase 0))
  (define (all-halted?) (vector-ref *halt-table* 0))
  (define *output* '())
  (define machine-a #f)
  (define (try)
    ;;
    (set! *thrusters* 0) 
    (set! *halt-table* (make-vector 5 #f))

    (set! machine-a (make-machine ident: 0 tapelist: tapelist relbase: relbase))
    ;; set first value in mailbox to zero (0)
    (vector-set! *mailbox-table* 0 input)
    (vector-set! *mailbox-table* 1 #f)
    ;;
    (let loop ()
      (machine-a 'step)
      ;; read mailbox 1 for any output
      (let ((mail (vector-ref *mailbox-table* 1)))
	(when (integer? mail)
	  ;;(format #t "OUT ~a~%" mail)
	  (set! *output* (cons mail *output*))
	  ;; empty mailbox 
	  (vector-set! *mailbox-table* 1 #f)
	  ))
      (format #t "machine-a : ip ~a : relbase ~a~%"
	      (machine-a 'ip)
	      (machine-a 'relbase))      
      ;; check if machine-a has halted 
      (cond
       ((all-halted?)
	(format #t "machine-a has halted : ")
	(format #t "ip = ~a~%" (machine-a 'ip))
	)
       (else (loop)))))
  (try)
  (values (reverse *output*)
	  machine-a	  
	  ))
;;(cons (reverse *output*) machine-a))




(define (ex1)
    (parameterize ((*verbose* #f))	   
      (run 
	   tapelist: '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))))


(define (ex2)
    (parameterize ((*verbose* #f))	   
      (run phase: 0
	   tapelist: '(1102 34915192 34915192 7 4 7 99 0))))

(define (ex3)
    (parameterize ((*verbose* #f))	   
      (run phase: 0
	   tapelist: '(104 1125899906842624 99))))


;; named arguments 
;; optional named parameters ? begin with *relative-base* of 2000 instead
;; outputs whatever is at 1985 in memory
(define (ex4)
    (parameterize ((*verbose* #t))	   
      (run phase: 696969
	   relbase: 2000
	   tapelist: '(
		       103 1985 ;; should put 696969 into memory adddress 1985
		       109 19 204 -34 99))))


;; convert day5 tests - may also include day 7 tests 
;; (define-syntax one-read-write-example
;;   (syntax-rules ()
;;     ((_ title in expect tape)
;;      ;; put test at start and compute out value - if compute crashes it will be caught!
;;      (test title expect 
;;        (let ((out #f)
;; 	     (ip 0))
;; 	 (letrec ((reader (lambda (sym)
;; 			    (cond
;; 			     ((eq? sym 'get) in)
;; 			     (else (error "reader error ")))))
;; 		  (writer (lambda (w) (set! out w))))
;; 	   ;; *verbose* keeps interpret quiet so we can see results of test
;; 	   (parameterize ((*verbose* #f))
;; 	     (interpret-tape (list->vector tape) ip reader writer))
;; 	   out))))))


;; this actually starts the test before machine has been started
;; so captures any really bad exceptions and all
(define-syntax one-read-write-example
  (syntax-rules ()
    ((_ title in expect tape)
     (test title expect 
	   (parameterize ((*verbose* #f))	   
	     (let ((out (run phase: in ;; a single input 
			     tapelist: tape)))
	       (assert (= (length out) 1))
	       (first out)))))))

;;(run phase: 8 tapelist: '(3 9 8 9 10 9 4 9 99 -1 8))
(call-with-values (lambda () (values 1 2 3))
		  (lambda (a b c) (list a b)))

;; need to convert machine tape (or memory of machine as hashtable) to a list
(define (machine-tape->list ht)
  (assert (hash-table? ht))
  ;; (format #t "hash table size =~a~%" (hash-table-size ht))
  ;; (hash-table-walk ht
  ;; 		   (lambda (k v)
  ;; 		     (format #t "key = ~a : val = ~a ~%" k v)))
  (let ((out '()))
    (let loop ((i (+ -1 (hash-table-size ht))))
      (set! out (cons (hash-table-ref ht i) out))
      ;; (format #t "out => ~a~%" out)
      (when (> i 0) (loop (- i 1)))
      out)))


(define (day2test)
  (test-begin "day2")  
    (test "day2ex1"  '(3500 9 10 70 2 3 11 0 99 30 40 50)
	  (let ((tape '(1 9 10 3 2 3 11 0 99 30 40 50)))
	    (parameterize ((*verbose* #f))
	      (call-with-values (lambda () (run input: 0 tapelist: tape))
		(lambda (out machine)
		  (format #t "out => ~a~%" out)
		  (format #t "machine => ~a~%" machine)
		  (machine-tape->list (machine 'tape))
		  )))))
  (test-end "day2"))


(define (day5test)
  (test-begin "day5")
  (let ((tape '(3 9 8 9 10 9 4 9 99 -1 8)))       
    (one-read-write-example "input equal to 8 : output 1 else 0" 8 1 tape)
    (one-read-write-example "input equal to 8 : output 1 else 0" 7 0 tape))	 
  (let ((tape '(3 9 7 9 10 9 4 9 99 -1 8)))	
    (one-read-write-example "input less than 8 ? output 1 else 0 " 7 1 tape)
    (one-read-write-example "input less than 8 ? output 1 else 0 " 8 0 tape)
    )
  (let ((tape '(3 3 1108 -1 8 3 4 3 99)))	
    (one-read-write-example "immediate mode : equal to 8; output 1 else 0." 7 0 tape)
    (one-read-write-example "immediate mode : equal to 8; output 1 else 0." 8 1 tape)
    )  
(let ((tape '(3 3 1107 -1 8 3 4 3 99 )))	
    (one-read-write-example "immediate mode : less than 8; output 1 else 0 " 7 1 tape)
    (one-read-write-example "immediate mode : less than 8; output 1 else 0 " 8 0 tape)
    )
(let ((tape '(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)))	
    (one-read-write-example "jump tests position mode - output 0 if input 0 else 1 nonzero " 0 0 tape)
    (one-read-write-example "jump tests position mode - output 0 if input 0 else 1 nonzero " 1 1 tape)
    )
(let ((tape '(3 3 1105 -1 9 1101 0 0 12 4 12 99 1)))	
    (one-read-write-example "jumptests immediate mode - output 0 if input 0 else 1 nonzero " 0 0 tape)
    (one-read-write-example "jumptests immediate mode - output 0 if input 0 else 1 nonzero " 1 1 tape)
    )
;; Here's a larger example:
;; 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99
;; The above example program uses an input instruction to ask for a single number. The program will then output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8.

(let ((tape '(3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99)))	
    (one-read-write-example "larger example : input less 8 output 999 " 7 999 tape)
    (one-read-write-example "larger example : input equal 8 output 1000 " 8 1000 tape)
    (one-read-write-example "larger example : input greater 8 output 1001 " 9 1001 tape)
    )
  (test-end "day5")
  )

;;(day5test)

(define (day9test)  
  (test "ex1" '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99) (ex1))
  (test "ex2" '(1219070632396864) (ex2))
  (test "ex3" '(1125899906842624) (ex3)))

  

(define (day9part1)
    (parameterize ((*verbose* #t))	   
      (run phase: 1
	   tapelist: day9input)))

