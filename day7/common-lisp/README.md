# common-lisp
### _Your Name <your.name@example.com>_

This is a project to do ... something.

## License

Specify license here

## 

```
;; navigate to the asd file 
;; in slime C-c C-k
;;
;; (ql:quickload :aoc-test)

(in-package :aoc-test)

;; run root test suite that runs everything  , one line per pass or fail
(run 'my-system)

;; run part of test suite only called foo-bar
(run 'foo-bar)

;; run tests and show full failures
(run! 'my-system)
(run! 'foo-bar)

```




