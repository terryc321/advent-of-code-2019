;; -*- geiser-scheme-implementation: chicken -*-

(import (chicken format))
(import (chicken process-context)) ;; change-directory current-directory
(import procedural-macros)
(import srfi-1)
;;(import srfi-69) ;; hash-tables
;; native support for records 
(import srfi-9) 
(import test) ;; testing framework
(import (chicken base)) ;; do i need to do this ?

;; +1 access to easy documentation
;; we can see all about the imported test module using 
;; >,doc (test) 
(test 4 (+ 2 2))
;; group
(test-group "A group"
  (test "A test with description" 5 (+ 2 3))
  (test-assert "This should always be true" (string? "foo")))

(begin 
  (test-group "B group"
    (test "B test with description" 6 (+ 2 4))
    (test "B test with description2" 7 (+ 2 5))
    (test "B test with description3" 8 (+ 3 5))
    (test "B test with description4" 0 (+ 3 5))
    (test "B test with description5" 999 (/ 1 0))
    )
  'done)


(begin 
  (test-begin "fooC")
  (test "C test with description" 6 (+ 2 4))
  (test "C test with description2" 7 (+ 2 5))
  (test-end "fooC")
  (test-begin "fooC2")
  (test "C test with description3" 8 (+ 3 5))
  (test "C test with description4" 0 (+ 3 5))
  (test "C test with description5" 999 (/ 1 0))
  (test-end "fooC2")  
  'done)

;; here we have multiple groups and multiple begin-end pairs - have groups 
(begin
  (test-group "D1 group"
  (test-begin "fooD")
  (test "D test with description" 6 (+ 2 4))
  (test "D test with description2" 7 (+ 2 5))
  (test-end "fooD")
  (test-begin "fooD2")
  (test "D test with description3" 8 (+ 3 5))
  (test "D test with description4" 0 (+ 3 5))
  (test "D test with description5" 999 (/ 1 0))
  (test-end "fooD2"))
  (test-group "E2 group"
    (test-begin "fooE")
    (test "E test with description" 6 (+ 2 4))
    (test "E test with description2" 7 (+ 2 5))
    (test-end "fooE")
    (test-begin "fooE2")
    (test "E test with description3" 8 (+ 3 5))
    (test "E test with description4" 0 (+ 3 5))
    (test "E test with description5" 999 (/ 1 0))
    (test-end "fooE2"))'done)

;; miss out on documentation opportunity if we use anonymous functions
(define mytest1
  (lambda ()
    "test title expected-result the-computation"
    (test "a random test" 3 (/ 4 0))))

;; Calculates the factorial of a non-negative integer.
;;
;; Arguments:
;;   n - The number to calculate the factorial for.
;; Returns:
;;   The factorial of n.
(define (mytest2)
  ;; this is inside mytest2
  "doc title for mytest2 - put something pithy here"
  (test "another random test" 5 (error "foo hit my head")))

mytest2

;;(test-exit) ;; (test-exit kills 

;; (test-group "B group"
;;   (test "add" (+ 1 2) 3)
;;   )

;; (test-begin "foo")
;; (test "adder" (+ 1 2) 3)
;; (test-end)




;; (test-group "foo"
;;   (test-begin "Math operations")
;;   (test "addition" (+ 1 2) 3))
;;   ;;(test "subtraction" (- 5 2) 3)
;;   (test-end)

