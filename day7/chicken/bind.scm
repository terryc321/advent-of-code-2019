;; -*- geiser-scheme-implementation: chicken -*-

(import bindings)
(import (chicken format))

;; https://wiki.call-cc.org/eggref/5/bindings
(define (run)
  (bind (a b c) '(1 2 3)
	(format #t "a => ~a : b => ~a : c => ~a ~%" a b c)))
