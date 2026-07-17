;; -*- geiser-scheme-implementation: chicken -*-

#|

machine interprets can run until either it needs a read - of which is not a phase
- it has already read once - the phase is used up
- it has to wait on

 machine a : phase-a input(1)=0 input(n)= from machine e

if we give each machine an ident
machine-a has ident 0
machine-b has ident 1
and so on

we can have idea of mailboxes

0   1          2    3     4      5
#f  a-number   #f   #f    #f     #f

so we can see if
each machine can continue processing until such time as the slot it will write to
already has a number
machine e with ident 5 here is the last machine


|#

  


(import (chicken format))
(import (chicken process-context)) ;; change-directory current-directory
(import procedural-macros)
(import srfi-1)
;;(import srfi-69) ;; hash-tables
;; native support for records 
(import srfi-9) 
(import test) ;; testing framework
(import (chicken base)) ;; do i need to do this ?
(import srfi-39) ;; dynamic variables using parameterise 
(import bindings) ;; (bind (a b c) '(1 2 3) ...


(define day7input '(
3 8 1001 8 10 8 105 1 0 0 21 46 67 76 97 118 199 280 361 442 99999 3 9 1002 9 3 9 101 4 9 9 102 3 9 9 1001 9 3 9 1002 9 2 9 4 9 99 3 9 102 2 9 9 101 5 9 9 1002 9 2 9 101 2 9 9 4 9 99 3 9 101 4 9 9 4 9 99 3 9 1001 9 4 9 102 2 9 9 1001 9 4 9 1002 9 5 9 4 9 99 3 9 102 3 9 9 1001 9 2 9 1002 9 3 9 1001 9 3 9 4 9 99 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 99 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 99	      ))


(define *verbose* (make-parameter #t))

;; halt-table #f means not yet halted
(define *halt-table* (make-vector 5 #f))

;; initial phases - this is set by permutation 
(define *phase-table* (make-vector 5 #f))

;; mail-table #f means no mail for that respective machine yet
;; a machine has to wait to write into the mailbox 
(define *mailbox-table* (make-vector 5 #f))

(define *thrusters* 0)

(define-syntax %param
  (syntax-rules ()
    ((_ tape p arg) (cond
		     ((= p 0) (vector-ref tape arg))
		     ((= p 1) arg)
		     ((= p 2) (vector-ref tape (+ arg *relative-base*)))
		     (else (error "%param mode expected 0 1 2"))))))

;; we can use the function as a pure procedure jump
;;        (lambda (tape ip reader writer ident)
(define-syntax binary-operation
  (syntax-rules ()
    ((_ name op tape ip)
     (define (name)
	 (let* ((arg1 (vector-ref tape (+ ip 1)))
		(arg2 (vector-ref tape (+ ip 2)))
		(arg3 (vector-ref tape (+ ip 3)))
		(p1 (param-1 (vector-ref tape ip)))
		(p2 (param-2 (vector-ref tape ip)))
		;; p3 not required as its writing	  
		(v1 (%param tape p1 arg1))
		(v2 (%param tape p2 arg2))
		(v3 arg3)
		)
	   (vector-set! tape v3 (op v1 v2))
	   (set! ip (+ ip 4)))))))

(define-syntax conditional-binary-operation
  (syntax-rules ()
    ((_ name op tape ip)
     (define (name)
	 (let* ((arg1 (vector-ref tape (+ ip 1)))
		(arg2 (vector-ref tape (+ ip 2)))
		(arg3 (vector-ref tape (+ ip 3)))
		(p1 (param-1 (vector-ref tape ip)))
		(p2 (param-2 (vector-ref tape ip)))
		;; p3 not required as its writing	  
		(v1 (%param tape p1 arg1)) ;;(if (zero? p1) (vector-ref tape arg1) arg1))
		(v2 (%param tape p2 arg2)) ;;(if (zero? p2) (vector-ref tape arg2) arg2))
		(v3 arg3)
		)
	   ;; condition
	   (cond
	    ((op v1 v2) (vector-set! tape v3 1))
	    (else (vector-set! tape v3 0)))
	   ;; next
	   (set! ip (+ ip 4)))))))

(define-syntax conditional-jump
  (syntax-rules ()
    ((_ name op tape ip)
     (define (name)
       (let* ((arg1 (vector-ref tape (+ ip 1)))
	      (arg2 (vector-ref tape (+ ip 2)))
	      (p1 (param-1 (vector-ref tape ip)))
	      (p2 (param-2 (vector-ref tape ip)))	   
	      (v1 (%param tape p1 arg1)) ;;(if (zero? p1) (vector-ref tape arg1) arg1))
	      (v2 (%param tape p2 arg2)) ;;(if (zero? p2) (vector-ref tape arg2) arg2))
	      )
	 (cond
	  ((op v1)
	   ;; set ip to 2nd parameter
	   (set! ip v2))
	  (else
	   ;; skip over
	   (set! ip (+ ip 3)))))))))


;; =====================================================

#|

  (define (i_add) .. does an add)
  (define (i_multiply) ... does a multiply 

 initial setup place a zero (0) in mailbox for machine A 

a machine can be stuck on a READ awaiting for a number to
be placed in its consumer mailbox

a machine can be stuck on a WRITE awaiting for a false #f to
be placed in its producer mailbox

   ...consumer mailbox -> [ machine ] -> .. producer mailbox ..

a machine can be halted

a machine can be working 

|#

(define (without n xs)
  (filter (lambda (m) (not (= n m))) xs))

;; able to generate 120 possible permutations correctly , any order is fine .
(define (perms as fn)
  (let ((count 1))
    (letrec ((perm (lambda (xs ys)
		     (cond
		      ((null? xs)
		       (fn ys)
		       (format #t "trying permutation ~a => ~a~%" count ys)
		       (set! count (+ 1 count)))
		      (else
		       (map (lambda (n) (perm (without n xs) (cons n ys))) xs)
		       #f)))))
      (perm as '()))))


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
   ((= op 5) "jump-if-true")
   ((= op 6) "jump-if-false")
   ((= op 7) "less-than")
   ((= op 8) "equals")
   ((= op 99) "halt")   
   (else (format #f "unknown-~a" op))))


#|
machine 
|#
(define (make-machine ident tapelist)
  (let ((ip 0)
	(tape (list->vector tapelist))
	(status 'inactive)
	(halted #f))
    
    (binary-operation i_add + tape ip)
    (binary-operation i_multiply * tape ip)
    (conditional-binary-operation i_less_than < tape ip)     
    (conditional-binary-operation i_equals = tape ip)
    (conditional-jump i_jump_true (lambda (v) (not (zero? v))) tape ip)
    (conditional-jump i_jump_false zero? tape ip)

    ;; reading phase will always succeed - its there at outset (hopefully..)
    ;; 
    (define i_read ;; more complex local state - first read comes from phase table 
      (let ((reads 0))
	(lambda ()
	  (cond
	   ((< reads 1)
	    (set! reads (+ 1 reads))
	    (set! in (vector-ref *phase-table* ident))
	    (let* ((arg1 (vector-ref tape (+ ip 1)))
		   (p1 (param-1 (vector-ref tape ip)))
		   (v1 arg1))		   
	      (set! status 'working)
	      (vector-set! tape v1 in)
	      (set! ip (+ ip 2))))
	   ((equal? #f (vector-ref *mailbox-table* ident)) (set! status 'block-read))
	   (else ;; assume its a number
	    (let* ((arg1 (vector-ref tape (+ ip 1)))
		   (p1 (param-1 (vector-ref tape ip)))
		   (v1 arg1)
		   (in (vector-ref *mailbox-table* ident)))
	      (set! status 'working)
	      ;; reset inbox to empty as taken a value
	      (vector-set! *mailbox-table* ident #f)
	      ;; change tape and continue 
	      (when (*verbose*)  (format #t "read in ~a ~%" in))      
	      (vector-set! tape v1 in)
	      (set! ip (+ ip 2))))))))

    ;;  0 1 2 3 4
    (define i_write 
      (lambda ()
	(let ((destination (+ ident 1))
	      (to-thrusters #f))
	  (when (= destination 5)
	    (set! to-thrusters #t) 
	    (set! destination 0))
	(cond
	 ((integer? (vector-ref *mailbox-table* destination))
	  (set! status 'block-write))
	 (else ;; assume destination mailbox empty
	  (let* ((arg1 (vector-ref tape (+ ip 1)))
		 (p1 (param-1 (vector-ref tape ip)))
		 (v1 (cond
		      ((= p1 0) (vector-ref tape arg1))
		      ((= p1 1) arg1)
		      ((= p1 2) (vector-ref tape arg1)))))
	    (vector-set! *mailbox-table* destination v1)
	    ;; thunderbirds are go !!
	    (when to-thrusters (set! *thrusters* v1))
	    (set! ip (+ ip 2))))))))
    
    
    (define (i_halt)
      (set! halted #t)
      (vector-set! *halt-table* ident #t))
    (define (i_step)
      (cond
       (halted #f)
       (else 
	(let ((op (param-op (vector-ref tape ip))))
	  (when (*verbose*)
	    (format #t "step (ip)~a : (op)~a:~a ~%" ip op (op-to-string op)))
	  (cond
	   ((= op 1)  (i_add))  
	   ((= op 2)  (i_multiply)) 
	   ((= op 3)  (i_read))  ;;
	   ((= op 4)  (i_write)) ;;
	   ((= op 5)  (i_jump_true)) 
	   ((= op 6)  (i_jump_false))
	   ((= op 7)  (i_less_than)) 
	   ((= op 8)  (i_equals))    
	   ((= op 99) (i_halt))     
	   (else (error "unknown opcode")))))))
    (lambda (cmd . args)
      (cond
       ((equal? cmd 'ip) ip)
       ((equal? cmd 'step) (i_step))
       ((equal? cmd 'halted?) halted)
       ((equal? cmd 'status) status)))))



;; perms '(5 6 7 8 9) fn => try each perm given 
(define (run tapelist)
  (define best 0)
  (define best-combo 0)
  (define (all-halted?)
    (and (vector-ref *halt-table* 0)
	 (vector-ref *halt-table* 1)
	 (vector-ref *halt-table* 2)
	 (vector-ref *halt-table* 3)
	 (vector-ref *halt-table* 4)))
  
  (define (try-perm phases)
    ;;
    (set! *thrusters* 0) 
    (set! *halt-table* (make-vector 5 #f))
    (set! *phase-table* (make-vector 5 #f))
    
    ;; the five machines -- internal defines ? may be converted to letrecs ..
    (define machine-a (make-machine 0 tapelist))
    (define machine-b (make-machine 1 tapelist))
    (define machine-c (make-machine 2 tapelist))
    (define machine-d (make-machine 3 tapelist))
    (define machine-e (make-machine 4 tapelist))
    ;; distribute phases to each machine phase bank 
    (bind (phase-a phase-b phase-c phase-d phase-e)  phases
	  (vector-set! *phase-table* 0 phase-a)
	  (vector-set! *phase-table* 1 phase-b)
	  (vector-set! *phase-table* 2 phase-c)
	  (vector-set! *phase-table* 3 phase-d)
	  (vector-set! *phase-table* 4 phase-e))
    ;; set first value in mailbox to zero (0)
    (vector-set! *mailbox-table* 0 0)
    ;;
    (let loop ()
      (machine-a 'step)
      (machine-b 'step)
      (machine-c 'step)
      (machine-d 'step)
      (machine-e 'step)
      ;; (format #t "status : ~a : ~a : ~a : ~a : ~a ~% halt table : ~a ~%"
      ;; 	      (machine-a 'ip)
      ;; 	      (machine-b 'ip)
      ;; 	      (machine-c 'ip)
      ;; 	      (machine-d 'ip)
      ;; 	      (machine-e 'ip)
      ;; 	      *halt-table*)
      ;; check if we have all halted then
      (cond
       ((all-halted?)
	(format #t "solution for ~a was ~a ~%" phases *thrusters*)
	(when (> *thrusters* best )
	  (set! best *thrusters*)
	  (set! best-combo phases)))
       (else (loop)))))
  (perms '(5 6 7 8 9) try-perm)
  (list 'best best 'and-best-combo best-combo))




(define (ex1)
  (run
   '(3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5)))

(define (ex2)
  (parameterize ((*verbose* #f))	   
    (run
     '(3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 
	 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 
	 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10))))

    
(define (day7part2)
  (parameterize ((*verbose* #f))	   
    (run day7input)))


#|

solution for (9 8 7 6 5) was 3710683 
trying permutation 1 => (9 8 7 6 5)
solution for (8 9 7 6 5) was 5546228 
trying permutation 2 => (8 9 7 6 5)
solution for (9 7 8 6 5) was 2840027 
trying permutation 3 => (9 7 8 6 5)
solution for (7 9 8 6 5) was 2950644 
trying permutation 4 => (7 9 8 6 5)
solution for (8 7 9 6 5) was 5591313 
trying permutation 5 => (8 7 9 6 5)
solution for (7 8 9 6 5) was 3737361 
trying permutation 6 => (7 8 9 6 5)
solution for (9 8 6 7 5) was 3678029 
trying permutation 7 => (9 8 6 7 5)
solution for (8 9 6 7 5) was 5513574 
trying permutation 8 => (8 9 6 7 5)
solution for (9 6 8 7 5) was 2873153 
trying permutation 9 => (9 6 8 7 5)
solution for (6 9 8 7 5) was 2852931 
trying permutation 10 => (6 9 8 7 5)
solution for (8 6 9 7 5) was 5493352 
trying permutation 11 => (8 6 9 7 5)
solution for (6 8 9 7 5) was 3639884 
trying permutation 12 => (6 8 9 7 5)
solution for (9 7 6 8 5) was 2568653 
trying permutation 13 => (9 7 6 8 5)
solution for (7 9 6 8 5) was 2679270 
trying permutation 14 => (7 9 6 8 5)
solution for (9 6 7 8 5) was 2503233 
trying permutation 15 => (9 6 7 8 5)
solution for (6 9 7 8 5) was 2483011 
trying permutation 16 => (6 9 7 8 5)
solution for (7 6 9 8 5) was 2669160 
trying permutation 17 => (7 6 9 8 5)
solution for (6 7 9 8 5) was 2538316 
trying permutation 18 => (6 7 9 8 5)
solution for (8 7 6 9 5) was 5581203 
trying permutation 19 => (8 7 6 9 5)
solution for (7 8 6 9 5) was 3727251 
trying permutation 20 => (7 8 6 9 5)
solution for (8 6 7 9 5) was 5515893 
trying permutation 21 => (8 6 7 9 5)
solution for (6 8 7 9 5) was 3662425 
trying permutation 22 => (6 8 7 9 5)
solution for (7 6 8 9 5) was 2931573 
trying permutation 23 => (7 6 8 9 5)
solution for (6 7 8 9 5) was 2800729 
trying permutation 24 => (6 7 8 9 5)
solution for (9 8 7 5 6) was 3994267 
trying permutation 25 => (9 8 7 5 6)
solution for (8 9 7 5 6) was 5829812 
trying permutation 26 => (8 9 7 5 6)
solution for (9 7 8 5 6) was 3123611 
trying permutation 27 => (9 7 8 5 6)
solution for (7 9 8 5 6) was 3234228 
trying permutation 28 => (7 9 8 5 6)
solution for (8 7 9 5 6) was 5874897 
trying permutation 29 => (8 7 9 5 6)
solution for (7 8 9 5 6) was 4020945 
trying permutation 30 => (7 8 9 5 6)
solution for (9 8 5 7 6) was 4530319 
trying permutation 31 => (9 8 5 7 6)
solution for (8 9 5 7 6) was 6365864 
trying permutation 32 => (8 9 5 7 6)
solution for (9 5 8 7 6) was 4824455 
trying permutation 33 => (9 5 8 7 6)
solution for (5 9 8 7 6) was 5938056 
trying permutation 34 => (5 9 8 7 6)
solution for (8 5 9 7 6) was 7446697 
trying permutation 35 => (8 5 9 7 6)
solution for (5 8 9 7 6) was 7773329 
trying permutation 36 => (5 8 9 7 6)
solution for (9 7 5 8 6) was 3417871 
trying permutation 37 => (9 7 5 8 6)
solution for (7 9 5 8 6) was 3528488 
trying permutation 38 => (7 9 5 8 6)
solution for (9 5 7 8 6) was 3953927 
trying permutation 39 => (9 5 7 8 6)
solution for (5 9 7 8 6) was 5067528 
trying permutation 40 => (5 9 7 8 6)
solution for (7 5 9 8 6) was 4117801 
trying permutation 41 => (7 5 9 8 6)
solution for (5 7 9 8 6) was 5178129 
trying permutation 42 => (5 7 9 8 6)
solution for (8 7 5 9 6) was 6431442 
trying permutation 43 => (8 7 5 9 6)
solution for (7 8 5 9 6) was 4577490 
trying permutation 44 => (7 8 5 9 6)
solution for (8 5 7 9 6) was 7491766 
trying permutation 45 => (8 5 7 9 6)
solution for (5 8 7 9 6) was 7818398 
trying permutation 46 => (5 8 7 9 6)
solution for (7 5 8 9 6) was 4904374 
trying permutation 47 => (7 5 8 9 6)
solution for (5 7 8 9 6) was 5964702 
trying permutation 48 => (5 7 8 9 6)
solution for (9 8 6 5 7) was 3945801 
trying permutation 49 => (9 8 6 5 7)
solution for (8 9 6 5 7) was 5781346 
trying permutation 50 => (8 9 6 5 7)
solution for (9 6 8 5 7) was 3140925 
trying permutation 51 => (9 6 8 5 7)
solution for (6 9 8 5 7) was 3120703 
trying permutation 52 => (6 9 8 5 7)
solution for (8 6 9 5 7) was 5761124 
trying permutation 53 => (8 6 9 5 7)
solution for (6 8 9 5 7) was 3907656 
trying permutation 54 => (6 8 9 5 7)
solution for (9 8 5 6 7) was 4497609 
trying permutation 55 => (9 8 5 6 7)
solution for (8 9 5 6 7) was 6333154 
trying permutation 56 => (8 9 5 6 7)
solution for (9 5 8 6 7) was 4791745 
trying permutation 57 => (9 5 8 6 7)
solution for (5 9 8 6 7) was 5905346 
trying permutation 58 => (5 9 8 6 7)
solution for (8 5 9 6 7) was 7413987 
trying permutation 59 => (8 5 9 6 7)
solution for (5 8 9 6 7) was 7740619 
trying permutation 60 => (5 8 9 6 7)
solution for (9 6 5 8 7) was 3418685 
trying permutation 61 => (9 6 5 8 7)
solution for (6 9 5 8 7) was 3398463 
trying permutation 62 => (6 9 5 8 7)
solution for (9 5 6 8 7) was 3986877 
trying permutation 63 => (9 5 6 8 7)
solution for (5 9 6 8 7) was 5100478 
trying permutation 64 => (5 9 6 8 7)
solution for (6 5 9 8 7) was 3955008 
trying permutation 65 => (6 5 9 8 7)
solution for (5 6 9 8 7) was 5080128 
trying permutation 66 => (5 6 9 8 7)
solution for (8 6 5 9 7) was 6301285 
trying permutation 67 => (8 6 5 9 7)
solution for (6 8 5 9 7) was 4447817 
trying permutation 68 => (6 8 5 9 7)
solution for (8 5 6 9 7) was 7393637 
trying permutation 69 => (8 5 6 9 7)
solution for (5 8 6 9 7) was 7720269 
trying permutation 70 => (5 8 6 9 7)
solution for (6 5 8 9 7) was 4741697 
trying permutation 71 => (6 5 8 9 7)
solution for (5 6 8 9 7) was 5866817 
trying permutation 72 => (5 6 8 9 7)
solution for (9 7 6 5 8) was 2846537 
trying permutation 73 => (9 7 6 5 8)
solution for (7 9 6 5 8) was 2957154 
trying permutation 74 => (7 9 6 5 8)
solution for (9 6 7 5 8) was 2781117 
trying permutation 75 => (9 6 7 5 8)
solution for (6 9 7 5 8) was 2760895 
trying permutation 76 => (6 9 7 5 8)
solution for (7 6 9 5 8) was 2947044 
trying permutation 77 => (7 6 9 5 8)
solution for (6 7 9 5 8) was 2816200 
trying permutation 78 => (6 7 9 5 8)
solution for (9 7 5 6 8) was 3146505 
trying permutation 79 => (9 7 5 6 8)
solution for (7 9 5 6 8) was 3257122 
trying permutation 80 => (7 9 5 6 8)
solution for (9 5 7 6 8) was 3682561 
trying permutation 81 => (9 5 7 6 8)
solution for (5 9 7 6 8) was 4796162 
trying permutation 82 => (5 9 7 6 8)
solution for (7 5 9 6 8) was 3846435 
trying permutation 83 => (7 5 9 6 8)
solution for (5 7 9 6 8) was 4906763 
trying permutation 84 => (5 7 9 6 8)
solution for (9 6 5 7 8) was 3048893 
trying permutation 85 => (9 6 5 7 8)
solution for (6 9 5 7 8) was 3028671 
trying permutation 86 => (6 9 5 7 8)
solution for (9 5 6 7 8) was 3617085 
trying permutation 87 => (9 5 6 7 8)
solution for (5 9 6 7 8) was 4730686 
trying permutation 88 => (5 9 6 7 8)
solution for (6 5 9 7 8) was 3585216 
trying permutation 89 => (6 5 9 7 8)
solution for (5 6 9 7 8) was 4710336 
trying permutation 90 => (5 6 9 7 8)
solution for (7 6 5 9 8) was 3241445 
trying permutation 91 => (7 6 5 9 8)
solution for (6 7 5 9 8) was 3110601 
trying permutation 92 => (6 7 5 9 8)
solution for (7 5 6 9 8) was 3836261 
trying permutation 93 => (7 5 6 9 8)
solution for (5 7 6 9 8) was 4896589 
trying permutation 94 => (5 7 6 9 8)
solution for (6 5 7 9 8) was 3640513 
trying permutation 95 => (6 5 7 9 8)
solution for (5 6 7 9 8) was 4765633 
trying permutation 96 => (5 6 7 9 8)
solution for (8 7 6 5 9) was 5859220 
trying permutation 97 => (8 7 6 5 9)
solution for (7 8 6 5 9) was 4005268 
trying permutation 98 => (7 8 6 5 9)
solution for (8 6 7 5 9) was 5793910 
trying permutation 99 => (8 6 7 5 9)
solution for (6 8 7 5 9) was 3940442 
trying permutation 100 => (6 8 7 5 9)
solution for (7 6 8 5 9) was 3209590 
trying permutation 101 => (7 6 8 5 9)
solution for (6 7 8 5 9) was 3078746 
trying permutation 102 => (6 7 8 5 9)
solution for (8 7 5 6 9) was 6421268 
trying permutation 103 => (8 7 5 6 9)
solution for (7 8 5 6 9) was 4567316 
trying permutation 104 => (7 8 5 6 9)
solution for (8 5 7 6 9) was 7481592 
trying permutation 105 => (8 5 7 6 9)
solution for (5 8 7 6 9) was 7808224 
trying permutation 106 => (5 8 7 6 9)
solution for (7 5 8 6 9) was 4894200 
trying permutation 107 => (7 5 8 6 9)
solution for (5 7 8 6 9) was 5954528 
trying permutation 108 => (5 7 8 6 9)
solution for (8 6 5 7 9) was 6323818 
trying permutation 109 => (8 6 5 7 9)
solution for (6 8 5 7 9) was 4470350 
trying permutation 110 => (6 8 5 7 9)
solution for (8 5 6 7 9) was 7416170 
trying permutation 111 => (8 5 6 7 9)
solution for (5 8 6 7 9) was 7742802 
trying permutation 112 => (5 8 6 7 9)
solution for (6 5 8 7 9) was 4764230 
trying permutation 113 => (6 5 8 7 9)
solution for (5 6 8 7 9) was 5889350 
trying permutation 114 => (5 6 8 7 9)
solution for (7 6 5 8 9) was 3503722 
trying permutation 115 => (7 6 5 8 9)
solution for (6 7 5 8 9) was 3372878 
trying permutation 116 => (6 7 5 8 9)
solution for (7 5 6 8 9) was 4098538 
trying permutation 117 => (7 5 6 8 9)
solution for (5 7 6 8 9) was 5158866 
trying permutation 118 => (5 7 6 8 9)
solution for (6 5 7 8 9) was 3902790 
trying permutation 119 => (6 5 7 8 9)
solution for (5 6 7 8 9) was 5027910 
trying permutation 120 => (5 6 7 8 9)
0.07s CPU time, 118620/4587 mutations (total/tracked), 0/1355 GCs (major/minor), maximum live heap: 13.98 MiB
(best 7818398 and-best-combo (5 8 7 9 6))

7818398 solution accepted first time.
wow. that was mad. 

|#



  


