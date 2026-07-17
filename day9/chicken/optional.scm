
;; chicken scheme native named and optional arguments #!optional and #!key

(define (create-user username #!optional (age 0) #!key (email #f) (active #t))
  (list username age email active))

;; Call with only required arg
(create-user "Alice") 
;; => ("Alice" 0 #f #t)

;; Call with optional arg
(create-user "Bob" 25) 
;; => ("Bob" 25 #f #t)

;; Call with named args
(create-user "Charlie" 30 email: "charlie@example.com" active: #f)
;; => ("Charlie" 30 "charlie@example.com" #f)

;; =========
(define (create-db #!key (pay #f) (username '()))
  (list pay username))

(create-db username: "fred" pay: 344)

;; ==========
(define (create-car #!key (age 0) (reg "") (tax 0))
  (list age reg tax))

(create-car)
(create-car age: 3)
(create-car tax: 3)





