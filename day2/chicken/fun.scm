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

(import srfi-1)
;;(import srfi-69) ;; hash-tables


(define foo 3)

(define day2input '(1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 10 23 2 10 23 27 1 27 6 31 1 13 31 35 1 13 35 39 1 39 10 43 2 43 13 47 1 47 9 51 2 51 13 55 1 5 55 59 2 59 9 63 1 13 63 67 2 13 67 71 1 71 5 75 2 75 13 79 1 79 6 83 1 83 5 87 2 87 6 91 1 5 91 95 1 95 13 99 2 99 6 103 1 5 103 107 1 107 9 111 2 6 111 115 1 5 115 119 1 119 2 123 1 6 123 0 99 2 14 0 0))

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

(define interpret
  (lambda (tape)
    (let ((ip 0))
      (cond
       ((list? tape) (interpret-vec (list->vector tape) ip))
       ((vector? tape) (interpret-vec tape ip))
       (else (error "expected tape to be list or vector"))))))


(define interpret-vec
  (lambda (tape ip)
    (assert (and (>= ip 0) (< ip (vector-length tape))))
    (let ((op (vector-ref tape ip)))
      (cond
       ((= op 1) (interpret-add tape ip))
       ((= op 2) (interpret-multiply tape ip))
       ((= op 99) (interpret-halt tape ip))
       (else (error "opcode not recognised"))))))

(define interpret-add
  (lambda (tape ip)
    (assert (and (>= ip 0) (< ip (vector-length tape))))
    (let ((arg1 (vector-ref tape (+ ip 1)))
	  (arg2 (vector-ref tape (+ ip 2)))
	  (arg3 (vector-ref tape (+ ip 3))))
      (let ((v1 (vector-ref tape arg1))
	    (v2 (vector-ref tape arg2)))
	(vector-set! tape arg3 (+ v1 v2))
	(interpret-vec tape (+ ip 4))))))

(define interpret-multiply ;; same as interpret-add except * rather + 
  (lambda (tape ip)
    (assert (and (>= ip 0) (< ip (vector-length tape))))
    (let ((arg1 (vector-ref tape (+ ip 1)))
	  (arg2 (vector-ref tape (+ ip 2)))
	  (arg3 (vector-ref tape (+ ip 3))))
      (let ((v1 (vector-ref tape arg1))
	    (v2 (vector-ref tape arg2)))
	(vector-set! tape arg3 (* v1 v2))
	(interpret-vec tape (+ ip 4))))))

(define interpret-halt
  (lambda (tape ip)
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


		 

    
