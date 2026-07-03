;; -*- mode: scheme; geiser-scheme-implementation: chicken -*-
;;

(import chicken.process-content)
;;(current-directory)
;;(change-directory "day6/chicken")


;; chat gpty assists
;; read lines from a file -- some directory adjustment maybe necessary
;; (getcwd) 
;; (chdir "day6/guile")

;; (debug-enable 'backtrace)
;; ,(debug-enable 'debug)

;; general list functions 
(use-modules (srfi srfi-1))

;; read-lines 
(use-modules (ice-9 rdelim))

;; string-split
(use-modules (ice-9 string-fun))

;; hash tables 
(use-modules (srfi srfi-69))

(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define input (map (lambda (line)
		     (string-split ;; "L3T)N5D"
		      line #\)))
		   (read-lines "../input.txt")))

(define check-input
  (every (lambda (s)
           (and (= (string-length (car s)) 3)
		(= (string-length (car (cdr s))) 3)))
       input))
;; => ("Hello" "World")

;; collect symbols and give them an index into an array , we dont know how long the
;; vector should be huh.
;;(define symbol-vector #f)
(define *symbols* (make-hash-table))



;; for each string pair in input - because each ABC)DEF meaning DEF will point to ABC
;; really ABC <- DEF relationship
(define (find-parents xs)
  (cond
   ((null? xs) #t)
   (else (let ((pair (car xs)))
	   (let ((parent (string->symbol (car pair)))
		 (child (string->symbol (car (cdr pair)))))
	     (format #t "~a <- ~a ~%" parent child)
	     (let ((known (hash-ref *symbols* parent '())))
	       (cond
		((member child known) #t)
		(else (hash-set! *symbols* parent (cons child known)))))
	     (find-parents (cdr xs)))))))

;;(find-parents input)

(define (sanity)
  ;; (let ((known (hash-ref *symbols* "asdf" 'not-found)))
  ;;   (format #t "sanity check = ~a ~%" known))
  (define ht (make-hash-table eq? symbol-hash))
  (hash-set! ht 'foo 3)
  (format #t "foo = ~a ~%" (hash-ref ht 'foo 4))
  )

(sanity)







