;; -*- mode: scheme; geiser-scheme-implementation: chicken -*-
;;

(import chicken.process-context)
;;(current-directory)
;;(change-directory "day6/chicken")


(import srfi-1)  ;; general list functions 
(import srfi-69) ;;hash-tables
(import srfi-9)  ;; records 

(import chicken.io)
(import chicken.string)
(import chicken.format)

(define input (map (lambda (line)
		     (string-split line ")"))
		   (call-with-input-file "../input.txt"
		     (lambda (port) 
		       (read-lines port)))))

(define check-input
  (every (lambda (s)
           (and (= (string-length (car s)) 3)
		(= (string-length (car (cdr s))) 3)))
       input))
;; => ("Hello" "World")

;; collect symbols and give them an index into an array , we dont know how long the
;; vector should be huh.
;;(define symbol-vector #f)
(define *orbit* (make-hash-table))

(define *links* '())

;; for each string pair in input - because each ABC)DEF meaning DEF will point to ABC
;; really ABC <- DEF relationship
;; prefixed all links with letter p to prevent them being interpreted as numbers 
(define (find-parents xs)
  (cond
   ((null? xs) #t)
   (else (let ((pair (car xs)))
	   (let ((parent (string->symbol (string-append "P" (car pair))))
		 (child (string->symbol (string-append "P" (car (cdr pair))))))
	     (format #t "~a <- ~a ~%" parent child)
	     (set! *links* (cons (list parent child) *links*))
	     (let ((known (hash-table-ref/default *orbit* parent '())))
	       (cond
		((member child known) #t)
		(else (hash-table-set! *orbit* parent (cons child known)))))
	     (find-parents (cdr xs)))))))

;; got a hash table *symbols with 1923 entries *

(find-parents input)

;; (define (sanity)
;;   ;; (let ((known (hash-ref *orbit* "asdf" 'not-found)))
;;   ;;   (format #t "sanity check = ~a ~%" known))
;;   (define ht (make-hash-table eq? symbol-hash))
;;   (hash-table-set! ht 'foo 3)
;;   (format #t "foo = ~a ~%" (hash-table-ref/default ht 'foo 4))
;;   )

;; (sanity)

(define *all-symbols* (make-hash-table))

(define (find-all-symbols xs)
  (cond
   ((null? xs) #t)
   (else (let ((pair (car xs)))
	   (let ((parent (string->symbol (string-append "P" (car pair))))
		 (child (string->symbol (string-append "P" (car (cdr pair))))))
	     (format #t "~a <- ~a ~%" parent child)
	     (hash-table-set! *all-symbols* parent #t)
	     (hash-table-set! *all-symbols* child #t)
	     (find-all-symbols (cdr xs)))))))

(find-all-symbols input)

(define *root-symbols* (make-hash-table))

(hash-table-for-each *all-symbols* 
		     (lambda (key value)
		       (hash-table-set! *root-symbols* key #t)))


(define (find-root-symbol)
  (hash-table-for-each *all-symbols* 
		       (lambda (key value)
			 (let ((moons (hash-table-ref/default *orbit* key #f)))
			   (when moons 
			     (map (lambda (moon)(hash-table-delete! *root-symbols* moon))
				  moons)
			     #t)))))

;; PCOM is the root 

(define (build-tree symbol)
  (let ((children (hash-table-ref/default *orbit* symbol #f)))
    (cond 
     ((not children) 
      ;; (format #t "toys out of pram ... ~a not found in orbit ?!?~%" symbol)
      ;; (error (format #f "bad - no orbit found for {~a}" symbol))
      ;; case 1 - no other moons for this symbol ?
      symbol)
     (#t  ;; children not false
      (cond 
       ;; case 1 - no other moons for this symbol
       ((null? children) symbol) 
       ;; case 2 - theres one or more - so build those trees and add my symbol onto front
       (#t (cons symbol (map build-tree children))))))))

(define root (build-tree 'PCOM))

;; how can i have a structure in chicken scheme ?
(define-record-type planet
  (make-planet name score moons)
  planet?
  (name   planet-name   planet-name-set!)
  (score  planet-score  planet-score-set!)
  (moons  planet-moons  planet-moons-set!))

(let ((planet (make-planet 'foo 3 '())))
  (list (planet-score planet)
	(planet-name planet)
	(planet-moons planet)))


;; convert tree to planet structures ?
;; (define (conv-tree s)
;;   (cond 
;;    ((symbol? s) (make-planet s 0 '()))
;;    (else (let (map conv-tree s))))

;; we can get a tree traversal and count each moons depth
(define (traverse t)
  (define count 0)
  (define (trav t c)
    (set! count (+ count c))
    (cond
     ((symbol? t) #f)
     (else 
      (let ((leafs (cdr t)))
	(map (lambda (leaf) (trav leaf (+ c 1))) leafs)
	#f))))
  (trav t 0)
  count)

(define part-1 (traverse root))

;; YOU 
;; SAN 
;; need to find a common parent that contains both YOU and SAN 
;; 

;; (define (leafs t)
;;   (define acc '())
;;   (define (trav t)
;;     (cond
;;      ((symbol? t) (set! acc (cons t acc)))
;;      (else 
;;       (let ((leafs (cdr t)))
;; 	(map trav leafs)
;; 	#f))))
;;   (trav t)
;;   acc)
(define (leafs-of target)
  (call/cc (lambda (exit)
     (define (linear xs)
       (cond
	((null? xs) #f)
	(else (let* ((pr (car xs))
		     (parent (car pr))
		     (child (car (cdr pr))))
		(cond
		 ((eq? target child) (exit parent))
		 (#t (linear (cdr xs))))))))
     (linear *links*))))



;; we are looking for PYOU and PSAN since we prefixed everything with letter 'P'
(define (parent-of target)
  (call/cc (lambda (exit)
     (define (linear xs)
       (cond
	((null? xs) #f)
	(else (let* ((pr (car xs))
		     (parent (car pr))
		     (child (car (cdr pr))))
		(cond
		 ((eq? target child) (exit parent))
		 (#t (linear (cdr xs))))))))
     (linear *links*))))


;;(parent-of root 'PYOU)
;;(parent-of root 'PSAN)


(define (ancestor-tree a)
  (let ((parent (parent-of a)))
    (cond
     ((not parent) (list a))
     (else (cons a (ancestor-tree parent))))))

;; (define (repeat f x n)
;;   (cond 
   


;; TODO 

(define (common-ancestor a b)
  (let ((pa (reverse (ancestor-tree a)))
	(pb (reverse (ancestor-tree b))))
    (define (tails)
      (cond
       ((equal? (car pa) (car pb))
	(cond
	 ((equal? (car (cdr pa)) (car (cdr pb)))
	  (set! pa (cdr pa))
	  (set! pb (cdr pb))
	  (tails))
	 (#t (list pa pb))))))
    (tails)))

(define result (common-ancestor 'PYOU 'PSAN))
(define orbit-a (reverse (first result)))
(define orbit-b (reverse (second result)))
;; orbit-a (PYOU P2MT PQ6S PHHY PRM7 ... PYCH)
;;                ^-- start here 
;; orbit-b (PSAN PC8V PDHH PQJS PSB9 PFCT ... PYCH)
;;                ^-- finish here
(define path (append (cdr (cdr orbit-a)) (cdr (reverse (cdr (cdr orbit-b))))))
;; (length path) => 522  rejected too low 
;; 524 ?? rejected too high
;; logically must be 523 .








   
 




