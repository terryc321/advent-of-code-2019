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
(define *verbose* (make-parameter #t))

;; halt-table #f means not yet halted
(define *halt-table* (make-vector 5 #f))

;; initial phases - this is set by permutation 
(define *phase-table* (make-vector 5 #f))

;; mail-table #f means no mail for that respective machine yet
;; a machine has to wait to write into the mailbox 
(define *mailbox-table* (make-vector 5 #f))

;; we can use the function as a pure procedure jump
;;        (lambda (tape ip reader writer ident)
(define-syntax binary-operation
  (syntax-rules ()
    ((_ name op)
     (define (name)
	 (let* ((arg1 (vector-ref tape (+ ip 1)))
		(arg2 (vector-ref tape (+ ip 2)))
		(arg3 (vector-ref tape (+ ip 3)))
		(p1 (param-1 (vector-ref tape ip)))
		(p2 (param-2 (vector-ref tape ip)))
		;; p3 not required as its writing	  
		(v1 (if (zero? p1) (vector-ref tape arg1) arg1))
		(v2 (if (zero? p2) (vector-ref tape arg2) arg2))
		(v3 arg3)
		)
	   (vector-set! tape v3 (op v1 v2))
	   (set! ip (+ ip 4)))))))

(define-syntax conditional-binary-operation
  (syntax-rules ()
    ((_ name op)
     (define (name)
	 (let* ((arg1 (vector-ref tape (+ ip 1)))
		(arg2 (vector-ref tape (+ ip 2)))
		(arg3 (vector-ref tape (+ ip 3)))
		(p1 (param-1 (vector-ref tape ip)))
		(p2 (param-2 (vector-ref tape ip)))
		;; p3 not required as its writing	  
		(v1 (if (zero? p1) (vector-ref tape arg1) arg1))
		(v2 (if (zero? p2) (vector-ref tape arg2) arg2))
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
    ((_ name op)
     (define name
       (lambda (tape ip reader writer ident)
	 (let* ((arg1 (vector-ref tape (+ ip 1)))
		(arg2 (vector-ref tape (+ ip 2)))
		(p1 (param-1 (vector-ref tape ip)))
		(p2 (param-2 (vector-ref tape ip)))	   
		(v1 (if (zero? p1) (vector-ref tape arg1) arg1))
		(v2 (if (zero? p2) (vector-ref tape arg2) arg2))
		)
	   (cond
	    ((op v1)
	     ;; set ip to 2nd parameter
	     (set! ip v2))
	    (else
	     ;; skip over
	     (set! ip (+ ip 3))))))))))


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
    
    (binary-operation i_add +)
    (binary-operation i_multiply *)
    (conditional-binary-operation i_less_than <)     
    (conditional-binary-operation i_equals =)
    (conditional-jump i_jump-true (lambda (v) (not (zero? v))))
    (conditional-jump i_jump-false zero?)

    ;; reading phase will always succeed - its there at outset (hopefully..)
    ;; 
    (define i_read ;; more complex local state - first read comes from phase table 
      (let ((reads 0))
	(lambda ()
	  (cond
	   ((< reads 1)
	    (set! reads (+ 1 reads))
	    (set! in (vector-ref *phase-table* ident)))
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
	(let ((destination (+ ident 1)))
	  (when (= destination 5)
	    (set! destination 0))
	(cond
	 ((integer? (vector-ref *mailbox-table* destination))
	  (set! status 'block-write))
	 (else ;; assume destination mailbox empty
	  (let* ((arg1 (vector-ref tape (+ ip 1)))
		 (p1 (param-1 (vector-ref tape ip)))
		 (v1 (if (zero? p1) (vector-ref tape arg1) arg1)))
	    (vector-set! *mailbox-table* destination v1)
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
	   ((= op 1) (i_add))  
	   ((= op 2) (i_multiply)) 
	   ((= op 3) (i_read))  ;;
	   ((= op 4) (i_write)) ;;
	   ((= op 5) (i_jump_true)) 
	   ((= op 6) (i_jump_false))
	   ((= op 7) (i_less_than)) 
	   ((= op 8) (i_equals))    
	   ((= op 99) (i_halt))     
	   (else (error "unknown opcode")))))))
    (lambda (cmd . args)
      (cond
       ((equal? cmd 'step) (i_step))
       ((equal? cmd 'halted?) halted)
       ((equal? cmd 'status) 'not-yet-implemented)))))


;; perms '(5 6 7 8 9) fn => try each perm given 
(define (run tapelist)
  (define best 0)
  (define best-combo 0)
  (define (try-perm phases)
    ;; 
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
      (format #t "status : ~a : ~a : ~a : ~a : ~a ~%"
	      (machine-a 'status)
	      (machine-b 'status)
	      (machine-c 'status)
	      (machine-d 'status)
	      (machine-e 'status))
      ;; check if we have all halted then 
      (loop))
    )
  (perms '(5 6 7 8 9) try-perm))


(define (ex1)
  (run
   '(3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5)))


    
    
    
	  
  
  


