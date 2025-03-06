;;;; common-lisp.asd

(asdf:defsystem #:aoc
  :description "Describe common-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ()
  :components ((:file "aoc-package")
               (:file "aoc")
	       ))



(asdf:defsystem #:aoc-test
  :description "Describe common-lisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:aoc #:fiveam)
  :components (
	       (:file "aoc-package")
               (:file "aoc")
	       (:file "aoc-test-package")
               (:file "aoc-test")
	       ))



