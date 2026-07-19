
(import chicken.format) ; format
(import srfi-1) ; lists 
(import srfi-69) ; hash
(import chicken.process-context) ;; change-directory current-directory
(import chicken.io) ;; read-line

;; macros

(define-syntax %href
  (syntax-rules ()
    ((_ hash key) (hash-table-ref hash key))))

(define-syntax %hdef
  (syntax-rules ()
    ((_ hash key default) (hash-table-ref/default hash key default))))

(define-syntax %vertical-portal%
  (syntax-rules ()
    ((_ g x y portals)
     (let ((v1 (%hdef g (list x y) #\space))
	   (v2 (%hdef g (list x (+ 1 y)) #\space))
	   (v3 (%hdef g (list x (+ 2 y)) #\space)))
       (when (and (uppercase-letter? v1)
		  (uppercase-letter? v2)
		  (char=? v3 #\.))
	 ;; (format #t "found portal at ~a~%" (list x (+ 2 y)))
	 (let ((portal (list (format #f "~a~a" v1 v2) 'up (list x (+ 2 y)))))
	   (hash-table-set! g (list x (+ 2 y)) #\^)	   
	   (set! portals (cons portal portals))))
       ;; 
       (when (and (char=? v1 #\.)
		  (uppercase-letter? v2)
		  (uppercase-letter? v3))
	 ;; (format #t "found portal at ~a~%" (list x (+ 2 y)))
	 (let ((portal (list (format #f "~a~a" v2 v3) 'down (list x y))))
	   (hash-table-set! g (list x y) #\v)
	   (set! portals (cons portal portals))))
       ))))


(define-syntax %horizontal-portal%
  (syntax-rules ()
    ((_ g x y portals)
     (let ((v1 (%hdef g (list x y) #\space))
	   (v2 (%hdef g (list (+ x 1) y) #\space))
	   (v3 (%hdef g (list (+ x 2) y) #\space)))
       (when (and (uppercase-letter? v1)
		  (uppercase-letter? v2)
		  (char=? v3 #\.))
	 (let ((portal (list (format #f "~a~a" v1 v2) 'left (list (+ x 2) y))))
	   (hash-table-set! g (list (+ x 2) y) #\<)	   
	   (set! portals (cons portal portals))))
       ;; 
       (when (and (char=? v1 #\.)
		  (uppercase-letter? v2)
		  (uppercase-letter? v3))
	 (let ((portal (list (format #f "~a~a" v2 v3) 'right (list x y))))
	   (hash-table-set! g (list x y) #\>)
	   (set! portals (cons portal portals))))
       ))))
       


;; (change-directory "day20/scheme")
;; (current-directory)

;; lets load in the grid
(define (read-file str)
  (call-with-input-file str
    (lambda (stream)
      (map string->list (read-lines stream)))
    #:text))

(define *puzzle-grid* (read-file "../input.txt"))

(define (show-list-grid xs)
  (map (lambda (line)
	 (format #t "~a~%" line))
       xs)
  #f)

;; ;; grid type ?
;; (define (make-grid g)
;;   (define hash (make-hash-table #:test equal?  ))
;;   (define row 0)
;;   (define col 0)
;;   (begin 
;;     (map (lambda (line)
;; 	   (format #t "~a~%" line)
;; 	   (set! col 0)
;; 	   (map (lambda (ch)
;; 		  (hash-table-set! hash (list col row) ch)
;; 		  (format #t "(~a,~a) <=> ~a ~%" col row ch)
;; 		  (set! col (+ col 1))
;; 		  ) line)
;; 	   (set! row (+ row 1)))
;; 	 g)
;;     (hash-table-set! hash 'width col)
;;     (hash-table-set! hash 'height row)  
;;     hash))

(define (make-grid g)
  (define (visible c)
    (cond
     ((equal? c #\space) #\*)
     (#t c)))
  (let ((hash (make-hash-table #:test equal?  ))
	(row 0)
	(col 0))
    (map (lambda (line)
	   (format #t "~a~%" line)
	   (set! col 0)
	   (map (lambda (ch)
		  (hash-table-set! hash (list col row) ch)
		  (format #t "(~a,~a) <=> ~a ~%" col row (visible ch))
		  (set! col (+ col 1))
		  ) line)
	   (set! row (+ row 1)))
	 g)
    (hash-table-set! hash 'width col)
    (hash-table-set! hash 'height row)
    (hash-table-set! hash 'portals (make-hash-table))
    hash))


(define (show-grid g)
  (let ((width (%href g 'width))
	(height (%href g 'height)))
    (let loop ((x 0)(y 0))
      (cond
       ((>= y height) #f)
       ((>= x width)
	(format #t "~%")
	(loop 0 (+ y 1)))
       (else
	(let ((v (%hdef g (list x y) #f)))
	  (when (not v)
	    (hash-table-set! g (list x y) #\space)
	    (set! v #\space))
	  (format #t "~a" v)
	  (loop (+ x 1) y)))))))


(define (uppercase-letter? ch)
  ;;(assert (character? ch))
  (and (char>=? ch #\A) (char<=? ch #\Z)))




(define (find-portals g)  
  (let ((portals '())
	(width (%href g 'width))
	(height (%href g 'height)))
    (let loop ((x 0)(y 0))
      (cond
       ((>= y height) #f)
       ((>= x width)  (loop 0 (+ y 1)))
       (else
	(%vertical-portal% g x y portals)
	(%horizontal-portal% g x y portals)
	(loop (+ x 1) y))))
    (hash-table-set! g 'portals portals)
    portals))
  

(define g (let ((hash (make-grid *puzzle-grid*)))
	    (find-portals hash)
	    hash))


