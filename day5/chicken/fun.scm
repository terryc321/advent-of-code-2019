;; -*- geiser-scheme-implementation: chicken -*-
;; be careful here , dont choose guile 
;; default to chicken as only have geiser-chicken installed

;; should probably put ~/.emacs.d/init.el into configs for geiser + geiser chicken
;; everything seems to be working great now
;; emacs - disable file local variables

;; (string-a
;; M-x geiser-show-logs
;; geiser-repl--output-filter

(import (chicken format))
(import (chicken process-context)) ;; change-directory current-directory

(import procedural-macros)

(import srfi-1)

;;(import srfi-69) ;; hash-tables

;; native support for records 
(import srfi-9) 


;; procedural-macros
(define-macro (swap! a b)
  (let ((tmp (gensym "tmp")))
    `(begin
       (set! ,tmp ,a)
       (set! ,a ,b)
       (set! ,b ,tmp))))

(define (test-swap) 
  (let ((a 1)
	(b 2))
    (list (list 'a a 'b b)
	  (begin (swap! a b) #f)
	  (list 'a a 'b b))))


(define foo 3)

(define day2input '(1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 10 23 2 10 23 27 1 27 6 31 1 13 31 35 1 13 35 39 1 39 10 43 2 43 13 47 1 47 9 51 2 51 13 55 1 5 55 59 2 59 9 63 1 13 63 67 2 13 67 71 1 71 5 75 2 75 13 79 1 79 6 83 1 83 5 87 2 87 6 91 1 5 91 95 1 95 13 99 2 99 6 103 1 5 103 107 1 107 9 111 2 6 111 115 1 5 115 119 1 119 2 123 1 6 123 0 99 2 14 0 0))

(define day5input '(3 225 1 225 6 6 1100 1 238 225 104 0 1001 152 55 224 1001 224 -68 224 4 224 1002 223 8 223 1001 224 4 224 1 224 223 223 1101 62 41 225 1101 83 71 225 102 59 147 224 101 -944 224 224 4 224 1002 223 8 223 101 3 224 224 1 224 223 223 2 40 139 224 1001 224 -3905 224 4 224 1002 223 8 223 101 7 224 224 1 223 224 223 1101 6 94 224 101 -100 224 224 4 224 1002 223 8 223 101 6 224 224 1 224 223 223 1102 75 30 225 1102 70 44 224 101 -3080 224 224 4 224 1002 223 8 223 1001 224 4 224 1 223 224 223 1101 55 20 225 1102 55 16 225 1102 13 94 225 1102 16 55 225 1102 13 13 225 1 109 143 224 101 -88 224 224 4 224 1002 223 8 223 1001 224 2 224 1 223 224 223 1002 136 57 224 101 -1140 224 224 4 224 1002 223 8 223 101 6 224 224 1 223 224 223 101 76 35 224 1001 224 -138 224 4 224 1002 223 8 223 101 5 224 224 1 223 224 223 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 1008 677 677 224 1002 223 2 223 1006 224 329 1001 223 1 223 8 677 226 224 102 2 223 223 1006 224 344 101 1 223 223 1107 226 226 224 1002 223 2 223 1006 224 359 1001 223 1 223 1108 677 226 224 102 2 223 223 1005 224 374 1001 223 1 223 1007 226 226 224 102 2 223 223 1006 224 389 1001 223 1 223 108 677 677 224 1002 223 2 223 1005 224 404 1001 223 1 223 1007 677 677 224 102 2 223 223 1005 224 419 1001 223 1 223 8 226 677 224 102 2 223 223 1005 224 434 101 1 223 223 1008 677 226 224 102 2 223 223 1006 224 449 1001 223 1 223 7 677 677 224 102 2 223 223 1006 224 464 1001 223 1 223 8 226 226 224 1002 223 2 223 1005 224 479 1001 223 1 223 7 226 677 224 102 2 223 223 1006 224 494 1001 223 1 223 7 677 226 224 1002 223 2 223 1005 224 509 101 1 223 223 107 677 677 224 102 2 223 223 1006 224 524 101 1 223 223 1007 677 226 224 102 2 223 223 1006 224 539 101 1 223 223 107 226 226 224 1002 223 2 223 1006 224 554 101 1 223 223 1008 226 226 224 102 2 223 223 1006 224 569 1001 223 1 223 1107 677 226 224 1002 223 2 223 1005 224 584 101 1 223 223 1107 226 677 224 102 2 223 223 1005 224 599 101 1 223 223 1108 226 677 224 102 2 223 223 1005 224 614 101 1 223 223 108 677 226 224 102 2 223 223 1005 224 629 101 1 223 223 107 226 677 224 102 2 223 223 1006 224 644 1001 223 1 223 1108 226 226 224 1002 223 2 223 1006 224 659 101 1 223 223 108 226 226 224 102 2 223 223 1005 224 674 101 1 223 223 4 223 99 226))


#|
1 , 2 or 99 opcodes
1 add 
2 multiply 
99 halt

later has parameter modes
later has reading / writing where different machines feed each other
how debug that ?
(define interpret (lambda ...))
define state of the machine

what composes the state , what makes up a possible machine state
the tape or code the machine runs - call it the TAPE 
the instruction point - call it the IP 
the current status of machine - is it running ? has it halted ? has it faultered
 RUN | HALT | ERR
 where ERR can be any number of
do we keep a history of the , if tape is immutable we could go forwards and reverse in
the execution of the tape  - may be incredibly inefficient or it could be slightly
worse performacnce , lets keep idea open - how do we code this flexibility ?
is the tape immutable ? lets try immutable ?

history - can simply cons each , simply because we lack a proper general purpose
sequence Seq in haskell ?

if we keep track of the code changes
jumps for example just change IP

(ip 0) -> (ip 1) -> (ip 2) ->

execute an add - we read in values 

|#

;; generic tape type -
;; if we change tape representation , we can

;; list tape
;; vector tape
;; some weird rb-tree immutable tape 

;; rather than (tape ip) if we say state
;; interpret(state) then we can add somethign to state without having to go in
;; and change everywhere tape ip are used ?
;; how do we error out ?


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




;; our own machine state
;; Define the record type
(define-record-type machine
  (make-machine tape ip reader writer)  ;; constructor tape initial ip 
  machine?             ;; predicate (checks if something is a coordinate)
  (tape machine-tape)  ;; accessor for tape
  (ip machine-ip)      ;; accessor for ip
  (reader machine-reader) ;; reader - where machine reads inputs from  
  (writer machine-writer) ;; writer - where machine writes outputs to 
  )

(define machine-op
  (lambda (m ip)
    (assert (machine? m))
    (assert (and (>= ip 0) (< ip (vector-length (machine-tape m)))))    
    (param-op (vector-ref (machine-tape m) ip))))

;; Create an instance
(define m1
  (let ((tape (list->vector '(1 0 0 3 99 0)))
	(ip 0)
	(reader (lambda () 1))
	(writer (lambda (w) (format #t "wrote ~a~%" w))))
    (make-machine tape ip reader writer)))

;; Usage
(machine? m1)          ;; => 
(machine? '(10 20))    ;; => #f  (It is a list, not a machine)
(machine? '#(10 20))   ;; => #f  (It is a vector, not a machine)

;; Access fields
(machine-tape m1)       ;; =>
(machine-ip m1)         ;; =>
(machine-reader m1)     ;; =>
(machine-writer m1)     ;; =>


;; machine tape - independent of - ip ? 
;; a default reader writer 
(define interpret
  (lambda (tape)
    (letrec ((reader (lambda (sym)
		       (cond
			((eq? sym 'get) 1))))
	     (writer (lambda (w) (format #t "wrote ~a ~%" w))))
      (cond
       ((list? tape) (interpret-tape (list->vector tape) 0 reader writer))
       ((vector? tape) (interpret-tape tape 0 reader writer))
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
  (lambda (tape ip reader writer)
    (let ((op (param-op (vector-ref tape ip))))
      (format #t "interpret-tape (ip)~a : (op)~a:~a ~%" ip op (op-to-string op))
      (cond
       ((= op 1) (interpret-add tape ip reader writer))
       ((= op 2) (interpret-multiply tape ip reader writer))
       ((= op 3) (interpret-reader tape ip reader writer))
       ((= op 4) (interpret-writer tape ip reader writer))

       ((= op 5) (interpret-jump-if-true tape ip reader writer)) 
       ((= op 6) (interpret-jump-if-false tape ip reader writer))
       ((= op 7) (interpret-less-than tape ip reader writer))
       ((= op 8) (interpret-equals tape ip reader writer))
       
       ((= op 99) (interpret-halt tape ip reader writer))
       (else (interpret-error tape ip reader writer))))))

;; advance = (number of args) + 1 
(define interpret-jump-if-true
  (lambda (tape ip reader writer)
    (let* ((arg1 (vector-ref tape (+ ip 1)))
	   (arg2 (vector-ref tape (+ ip 2)))
	   (p1 (param-1 (vector-ref tape ip)))
	   (p2 (param-2 (vector-ref tape ip)))	   
	   (v1 (if (zero? p1) (vector-ref tape arg1) arg1))
	   (v2 (if (zero? p2) (vector-ref tape arg2) arg2))
	   )
      (cond
       ((not (zero? v1))
	;; set ip to 2nd parameter
	(interpret-tape tape v2 reader writer))
       (else
	;; skip over
	(interpret-tape tape (+ ip 3) reader writer))))))


(define interpret-jump-if-false
  (lambda (tape ip reader writer)
    (let* ((arg1 (vector-ref tape (+ ip 1)))
	   (arg2 (vector-ref tape (+ ip 2)))
	   (p1 (param-1 (vector-ref tape ip))) 
	   (p2 (param-2 (vector-ref tape ip))) 
	   (v1 (if (zero? p1) (vector-ref tape arg1) arg1))
	   (v2 (if (zero? p2) (vector-ref tape arg2) arg2))
	   )
      (cond
       ((zero? v1)
	;; set ip to 2nd parameter
	(interpret-tape tape v2 reader writer))
       (else
	;; skip over
	(interpret-tape tape (+ ip 3) reader writer))))))


(define interpret-less-than
  (lambda (tape ip reader writer)
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
       ((< v1 v2)
	(format #t "comparing ~a < ~a ~%" v1 v2)
	(vector-set! tape v3 1))
       (else (vector-set! tape v3 0)))
      ;; next
      (interpret-tape tape (+ ip 4) reader writer))))



(define interpret-equals
  (lambda (tape ip reader writer)
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
       ((= v1 v2) (vector-set! tape v3 1))
       (else (vector-set! tape v3 0)))
      ;; next
      (interpret-tape tape (+ ip 4) reader writer))))




(define interpret-add
  (lambda (tape ip reader writer)
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
	(vector-set! tape v3 (+ v1 v2))
	(interpret-tape tape (+ ip 4) reader writer))))


(define interpret-multiply
  (lambda (tape ip reader writer)
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
	(vector-set! tape v3 (* v1 v2))
	(interpret-tape tape (+ ip 4) reader writer))))


;; no param modes since writing only
(define interpret-reader
  (lambda (tape ip reader writer)
    (let* ((arg1 (vector-ref tape (+ ip 1)))
	   (p1 (param-1 (vector-ref tape ip)))
	   (v1 arg1) ;;(if (zero? p1) (vector-ref tape arg1) arg1))
	   (in (reader 'get))
	   )
      (format #t "read in ~a ~%" in)
	(vector-set! tape v1 in)
	(interpret-tape tape (+ ip 2) reader writer))))

;; where does writing go to ?
(define interpret-writer
  (lambda (tape ip reader writer)
    (let* ((arg1 (vector-ref tape (+ ip 1)))
	   (p1 (param-1 (vector-ref tape ip)))
	   (v1 (if (zero? p1) (vector-ref tape arg1) arg1)))
	(writer v1)
	(interpret-tape tape (+ ip 2) reader writer))))


(define interpret-halt
  (lambda (tape ip reader writer)
    tape))


(define make-tape
  (lambda (xs)
    (assert (list? xs))
    (list->vector xs)))

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


;; reads in a value and writes out same value
(define (ex201)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 123)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 0 4 0 99)) ip reader writer)))

;; multiplies 3 by 33 and puts it into position 4 
(define (ex202)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 123)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(1002 4 3 4 33)) ip reader writer)))

;; 
(define (ex203)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 123)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(1101 100 -1 4 0)) ip reader writer)))



;; (3 9 8 9 10 9 4 9 99 -1 8) - Using position mode
;; consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
(define (ex204)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 8)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 9 8 9 10 9 4 9 99 -1 8)) ip reader writer)))

(define (ex205)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 7)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 9 8 9 10 9 4 9 99 -1 8)) ip reader writer)))



;; - Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
;; ;; (3 9 7 9 10 9 4 9 99 -1 8) - Using position mode  consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
(define (ex206)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 5)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 9 7 9 10 9 4 9 99 -1 8)) ip reader writer)))

(define (ex207)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 8))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 9 7 9 10 9 4 9 99 -1 8)) ip reader writer)))



;; - Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
(define (ex208)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 5)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 3 1108 -1 8 3 4 3 99)) ip reader writer)))

(define (ex209)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 8))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 3 1108 -1 8 3 4 3 99)) ip reader writer)))



;; (3 3 1107 -1 8 3 4 3 99)
;; - Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
(define (ex210)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 5)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 3 1107 -1 8 3 4 3 99)) ip reader writer)))

(define (ex211)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 8))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 3 1107 -1 8 3 4 3 99)) ip reader writer)))

;; Here are some jump tests that take an input, then output 0 if the input was zero or 1 if the input was non-zero:
;; (3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9) (using position mode)

(define (ex212)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 0)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)) ip reader writer)))

(define (ex213)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 1))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9)) ip reader writer)))


;; Here are some jump tests that take an input, then output 0 if the input was zero or 1 if the input was non-zero:
;; 3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (using immediate mode)


(define (ex214)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 0)
		      (else (error "reader error ")))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 3 1105 -1 9 1101 0 0 12 4 12 99 1)) ip reader writer)))

(define (ex215)  
  (letrec ((reader (lambda (sym)
		     (cond
		      ((eq? sym 'get) 1))))
	   (writer (lambda (w) (format #t "wrote ~a ~%" w)))
	   (ip 0))
    (interpret-tape (list->vector '(3 3 1105 -1 9 1101 0 0 12 4 12 99 1)) ip reader writer)))


;; Here's a larger example:
;; 3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
;; 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
;; 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
;; The above example program uses an input instruction to ask for a single number. The program will then output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8.

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
    (interpret-tape (list->vector day5input) ip reader writer)))
