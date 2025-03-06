;;;; aoc-test.lisp

(in-package #:aoc-test)

;; aoc::foo

(def-suite my-system
  :description "Test my system")

(def-suite foo-bar
  :description "Test the foo function."
  :in my-system)

(in-suite foo-bar)


(test foo-is-2
  (is (= (aoc::foo) 2)))

(test foo-is-3
  (is (= (aoc::foo) 3)))





