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


;; we can use the function as a pure procedure jump
;;        (lambda (tape ip reader writer ident)
(define-syntax interpret-binary-operation
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

(define-syntax interpret-conditional-binary-operation
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



(define-syntax interpret-conditional-jump
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
	     (interpret-tape tape v2 reader writer ident))
	    (else
	     ;; skip over
	     (interpret-tape tape (+ ip 3) reader writer ident)))))))))


(define day2input '(1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 10 23 2 10 23 27 1 27 6 31 1 13 31 35 1 13 35 39 1 39 10 43 2 43 13 47 1 47 9 51 2 51 13 55 1 5 55 59 2 59 9 63 1 13 63 67 2 13 67 71 1 71 5 75 2 75 13 79 1 79 6 83 1 83 5 87 2 87 6 91 1 5 91 95 1 95 13 99 2 99 6 103 1 5 103 107 1 107 9 111 2 6 111 115 1 5 115 119 1 119 2 123 1 6 123 0 99 2 14 0 0))

(define day5input '(3 225 1 225 6 6 1100 1 238 225 104 0 1001 152 55 224 1001 224 -68 224 4 224 1002 223 8 223 1001 224 4 224 1 224 223 223 1101 62 41 225 1101 83 71 225 102 59 147 224 101 -944 224 224 4 224 1002 223 8 223 101 3 224 224 1 224 223 223 2 40 139 224 1001 224 -3905 224 4 224 1002 223 8 223 101 7 224 224 1 223 224 223 1101 6 94 224 101 -100 224 224 4 224 1002 223 8 223 101 6 224 224 1 224 223 223 1102 75 30 225 1102 70 44 224 101 -3080 224 224 4 224 1002 223 8 223 1001 224 4 224 1 223 224 223 1101 55 20 225 1102 55 16 225 1102 13 94 225 1102 16 55 225 1102 13 13 225 1 109 143 224 101 -88 224 224 4 224 1002 223 8 223 1001 224 2 224 1 223 224 223 1002 136 57 224 101 -1140 224 224 4 224 1002 223 8 223 101 6 224 224 1 223 224 223 101 76 35 224 1001 224 -138 224 4 224 1002 223 8 223 101 5 224 224 1 223 224 223 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 1008 677 677 224 1002 223 2 223 1006 224 329 1001 223 1 223 8 677 226 224 102 2 223 223 1006 224 344 101 1 223 223 1107 226 226 224 1002 223 2 223 1006 224 359 1001 223 1 223 1108 677 226 224 102 2 223 223 1005 224 374 1001 223 1 223 1007 226 226 224 102 2 223 223 1006 224 389 1001 223 1 223 108 677 677 224 1002 223 2 223 1005 224 404 1001 223 1 223 1007 677 677 224 102 2 223 223 1005 224 419 1001 223 1 223 8 226 677 224 102 2 223 223 1005 224 434 101 1 223 223 1008 677 226 224 102 2 223 223 1006 224 449 1001 223 1 223 7 677 677 224 102 2 223 223 1006 224 464 1001 223 1 223 8 226 226 224 1002 223 2 223 1005 224 479 1001 223 1 223 7 226 677 224 102 2 223 223 1006 224 494 1001 223 1 223 7 677 226 224 1002 223 2 223 1005 224 509 101 1 223 223 107 677 677 224 102 2 223 223 1006 224 524 101 1 223 223 1007 677 226 224 102 2 223 223 1006 224 539 101 1 223 223 107 226 226 224 1002 223 2 223 1006 224 554 101 1 223 223 1008 226 226 224 102 2 223 223 1006 224 569 1001 223 1 223 1107 677 226 224 1002 223 2 223 1005 224 584 101 1 223 223 1107 226 677 224 102 2 223 223 1005 224 599 101 1 223 223 1108 226 677 224 102 2 223 223 1005 224 614 101 1 223 223 108 677 226 224 102 2 223 223 1005 224 629 101 1 223 223 107 226 677 224 102 2 223 223 1006 224 644 1001 223 1 223 1108 226 226 224 1002 223 2 223 1006 224 659 101 1 223 223 108 226 226 224 102 2 223 223 1005 224 674 101 1 223 223 4 223 99 226))

(define day7input '(
3 8 1001 8 10 8 105 1 0 0 21 46 67 76 97 118 199 280 361 442 99999 3 9 1002 9 3 9 101 4 9 9 102 3 9 9 1001 9 3 9 1002 9 2 9 4 9 99 3 9 102 2 9 9 101 5 9 9 1002 9 2 9 101 2 9 9 4 9 99 3 9 101 4 9 9 4 9 99 3 9 1001 9 4 9 102 2 9 9 1001 9 4 9 1002 9 5 9 4 9 99 3 9 102 3 9 9 1001 9 2 9 1002 9 3 9 1001 9 3 9 4 9 99 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 99 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 1 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 99	      ))

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



;; a default reader writer 
(define interpret
  (lambda (tape)
    (letrec ((reader (lambda (sym)
		       (cond
			((eq? sym 'get) 1))))
	     (writer (lambda (w)
		       (when (*verbose*) (format #t "wrote ~a ~%" w))
		       #f
		       )))
      (cond
       ((list? tape) (interpret-tape (list->vector tape) 0 reader writer ident))
       ((vector? tape) (interpret-tape tape 0 reader writer ident))
       (else (error "unknown tape format type"))))))

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

(define interpret-tape
  (lambda (tape ip reader writer ident)
    (let ((op (param-op (vector-ref tape ip))))
      (when (*verbose*)
	(format #t "interpret-tape (ip)~a : (op)~a:~a ~%" ip op (op-to-string op)))
      (cond
       ((= op 1) (interpret-add tape ip reader writer ident))
       ((= op 2) (interpret-multiply tape ip reader writer ident))
       ((= op 3) (interpret-reader tape ip reader writer ident))
       ((= op 4) (interpret-writer tape ip reader writer ident))

       ((= op 5) (interpret-jump-if-true tape ip reader writer ident)) 
       ((= op 6) (interpret-jump-if-false tape ip reader writer ident))
       ((= op 7) (interpret-less-than tape ip reader writer ident))
       ((= op 8) (interpret-equals tape ip reader writer ident))
       
       ((= op 99) (interpret-halt tape ip reader writer ident))
       (else (interpret-error tape ip reader writer ident))))))

(interpret-conditional-jump interpret-jump-if-true (lambda (v) (not (zero? v))))
(interpret-conditional-jump interpret-jump-if-false zero?)
(interpret-binary-operation interpret-add +)
(interpret-binary-operation interpret-multiply *)
(interpret-conditional-binary-operation interpret-less-than <)     
(interpret-conditional-binary-operation interpret-equals =)     


;; no param modes since writing only
(define interpret-reader
  (lambda (tape ip reader writer ident)
    (let* ((arg1 (vector-ref tape (+ ip 1)))
	   (p1 (param-1 (vector-ref tape ip)))
	   (v1 arg1) 
	   (in (reader 'get))
	   )
      (when (*verbose*)
	(format #t "read in ~a ~%" in))      
	(vector-set! tape v1 in)
	(interpret-tape tape (+ ip 2) reader writer ident))))

;; where does writing go to ?
(define interpret-writer
  (lambda (tape ip reader writer ident)
    (let* ((arg1 (vector-ref tape (+ ip 1)))
	   (p1 (param-1 (vector-ref tape ip)))
	   (v1 (if (zero? p1) (vector-ref tape arg1) arg1)))
	(writer v1)
	(interpret-tape tape (+ ip 2) reader writer ident))))

(define interpret-halt
  (lambda (tape ip reader writer ident)
    tape))


(define make-tape
  (lambda (xs)
    (assert (list? xs))
    (list->vector xs)))


;; ===== we need to figure out how to handle testing ==========
;; test - cases but we dont handle any errors - which crash tests and no way determine
;; if
;; 
(define (ex1) (interpret '(1 0 0 3 99)))
(define (ex2) (interpret '(1 9 10 3 2 3 11 0 99 30 40 50)))
(define (ex3) (interpret '(1 0 0 0 99)))
(define (ex4) (interpret '(2 3 0 3 99)))
(define (ex5) (interpret '(2 4 4 5 99 0)))
(define (ex6) (interpret '(1 1 1 4 99 5 6 0 99)))

(define (day2part1) (interpret day2input))
(define (day2part2)
  (call/cc
   (lambda (exit)
     (let loop-noun ((noun 0))
       (let loop-verb ((verb 0))
	 (let ((tape (list->vector day2input)))
	   (vector-set! tape 1 noun)
	   (vector-set! tape 2 verb)
	   (let ((out (interpret tape)))
	     (when (= (vector-ref out 0) 19690720)
	       (format #t "solution ~a~%" (list noun verb (+ (* 100 noun) verb)))
	       (exit #t))
	     ;;(format #t "out = ~a~%" out)
	     (cond
	      ((< verb 99) (loop-verb (+ verb 1)))))))
       (cond
	((< noun 99) (loop-noun (+ noun 1))))))))

;; ===========================================================
;; these tests dont have a macro for them yet -

;; reads in a value and writes out same value
(define (ex201)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 123)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 0 4 0 99)) ip reader writer ident)))

;; multiplies 3 by 33 and puts it into position 4 
(define (ex202)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 123)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(1002 4 3 4 33)) ip reader writer ident)))

;; 
(define (ex203)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 123)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(1101 100 -1 4 0)) ip reader writer ident)))

;; ==============================================================================

(define-syntax one-read-write-example
  (syntax-rules ()
    ((_ title in expect tape)
     ;; put test at start and compute out value - if compute crashes it will be caught!
     (test title expect 
       (let ((out #f)
	     (ip 0))
	 (letrec ((reader (lambda (sym)
			    (cond
			     ((eq? sym 'get) in)
			     (else (error "reader error ")))))
		  (writer (lambda (w) (set! out w))))
	   ;; *verbose* keeps interpret quiet so we can see results of test
	   (parameterize ((*verbose* #f))
	     (interpret-tape (list->vector tape) ip reader writer ident))
	   out))))))



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

;; generalized testing 
;;  for a given tape - guarantee fresh - each run (list->vector xs) for fresh vector
;;  sequence of (in -> out) (in -> out) (in -> out)
;;  in is value that reader always produces , out is value writer produces as result running

(define (day5part1) (interpret day5input))
		 
(define (day5part2)
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 5))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector day5input) ip reader writer ident)))



(define (without n xs)
  (filter (lambda (m) (not (= n m))) xs))

;; developed permutation recursion in poly standard ml a week before
;;(without 1 '(1 2 3))
;; (* lot time wasted on curried / non curried functions  *)
;; fun without (n : int)  (xs : int list) : int list = 
;;     List.filter (fn m => not (n = m)) xs
;; (* choice 0 1 2 3 4 *)

;; fun perms () = 
;;   let val count = ref 1 
;;       fun perm xs ys =  
;;       case xs of 
;;        [] => (print "permutation : " ; print (Int.toString (!count)) ; 
;;               print " : " ;
;; 	      printList (Int.toString) ys ; 
;; 	      count := (!count) + 1 )
;;        | _ => let 
;; 	      in List.map (fn n => perm (without n xs) (n :: ys)) xs ; () 
;; 	      end
;;   in perm [0,1,2,3,4] []
;;   end

;; able to generate 120 possible permutations correctly , any order is fine .
(define (perms as fn)
  (let ((count 1))
    (letrec ((perm (lambda (xs ys)
		     (cond
		      ((null? xs)
		       (fn ys)
		       ;;(format #t "permutation ~a => ~a~%" count ys)
		       (set! count (+ 1 count)))
		      (else
		       (map (lambda (n) (perm (without n xs) (cons n ys))) xs)
		       #f)))))
      (perm as '()))))


(define-syntax make-amplifier
  (syntax-rules ()
    ((_ tape phase-a input-a input-b)
     (set! input-b
       (let ((out #f)
	     (ip 0))
	 (letrec ((reader (let ((reads 0))
			    (lambda (sym)				
			      (cond
			       ((eq? sym 'get)
				(cond
				 ((zero? reads) (set! reads (+ reads 1)) phase-a)
				 (else input-a)))
			       (else (error "reader error "))))))
		  (writer (lambda (w) (set! out w))))
	   ;; *verbose* keeps interpret quiet so we can see results of test
	   (parameterize ((*verbose* #f))
	     (interpret-tape (list->vector tape) ip reader writer ident))
	   out))))))

	     


;; five amplifiers - feed one into another - first amplifier gets input 0
;; amplifiers have a reader that yields their phase setting
;;  followed by any other reads of previous amplifiers output 
(define (day7part1)
  (let ((best 0)
	(best-combo '()))
    (perms '(0 1 2 3 4)
     (lambda (five)
       (format #t "permutation => ~a~%" five)
       (bind (phase-a phase-b phase-c phase-d phase-e) five
	     (format #t "a => ~a : b => ~a : c => ~a : d => ~a : e => ~a~%"
		     phase-a phase-b phase-c phase-d phase-e)
	     ;; build 5 machines with correct readers and writer goes to next machine
	     ;; todo
	     (let ((tape day7input)
		   (input-a 0)
		   (input-b 0)
		   (input-c 0)
		   (input-d 0)
		   (input-e 0)
		   (input-f 0) ;; dummy amplifier that figures out maximum best
		   )
	       
	       ;; machines 1 thru 5
	       ;; machine 1 has phase of a , with input of 0 zero
	       (make-amplifier tape phase-a input-a input-b)
	       ;; input-b is set by previous amplifier
	       (make-amplifier tape phase-b input-b input-c)
	       (make-amplifier tape phase-c input-c input-d)
	       (make-amplifier tape phase-d input-d input-e)
	       (make-amplifier tape phase-e input-e input-f)
	       ;; input-f is overall output

	       (when (> input-f best)
		 (set! best input-f)
		 (set! best-combo five))
	       ))))
    (list 'best best 'with-combo best-combo)))

;; can we build one machine ? yes
;; can we keep the 

(define-syntax make-amplifier-with-writer
  (syntax-rules ()
    ((_ tape phase-a input-a input-b writer ident)
     (set! input-b
       (let ((out #f)
	     (ip 0))
	 (letrec ((reader (let ((reads 0))
			    (lambda (sym)				
			      (cond
			       ((eq? sym 'get)
				(cond
				 ((zero? reads) (set! reads (+ reads 1)) phase-a)
				 (else input-a)))
			       (else (error "reader error "))))))
		  ;;(writer (lambda (w) (set! out w)))
		  )
	   ;; *verbose* keeps interpret quiet so we can see results of test
	   (parameterize ((*verbose* #f))
	     (interpret-tape (list->vector tape) ip reader writer ident))
	   out))))))



(define (day7part2)
  (let ((best 0)
	(best-combo '()))
    (perms '(5 6 7 8 9)
	   (lambda (five)
	     ;; (format #t "permutation => ~a~%" five)
	     (bind (phase-a phase-b phase-c phase-d phase-e) five
		   (format #t "a => ~a : b => ~a : c => ~a : d => ~a : e => ~a~%"
			     phase-a phase-b phase-c phase-d phase-e)
		   ;; build 5 machines with correct readers and writer goes to next machine
		   ;; todo
		   (let ((tape day7input)
			 (input-a 0)
			 (input-b 0)
			 (input-c 0)
			 (input-d 0)
			 (input-e 0)
			 (input-f 0) ;; dummy amplifier that figures out maximum best
			 )

		     ;; lets make them loopy - how many times should we loop ?
		     ;; 
		     (let loop ((i 0))
		       ;; machines 1 thru 5
		       ;; machine 1 has phase of a , with input of 0 zero
		       (make-amplifier-with-writer tape-a phase-a input-a input-b)
		       ;; input-b is set by previous amplifier
		       (make-amplifier-with-writer tape-b phase-b input-b input-c)
		       (make-amplifier-with-writer tape-c phase-c input-c input-d)
		       (make-amplifier-with-writer tape-d phase-d input-d input-e)
		       (make-amplifier-with-writer tape-e phase-e input-e input-f)
		       ;; input-f is overall output
		       (set! input-a input-f)
		       (cond
			(#t (loop (+ i 1)))))

		     (when (> input-f best)
		       (set! best input-f)
		       (set! best-combo five))
		     ))))
    (list 'best best 'with-combo best-combo)))




