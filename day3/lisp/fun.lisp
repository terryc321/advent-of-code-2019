
(defpackage :aoc
  (:use :cl))
(in-package :aoc)

(declaim (optimize (speed 1)(debug 3)(space 1)))

(defparameter input
  (with-open-file (stream "../input" :direction :input)
    (read stream)))


;; both have 301 inputs 
(defun cnv (s)
  (let ((str (format nil "~a" s)))
    (let ((left (char= (char str 0) #\L))
	  (up (char= (char str 0) #\U))
	  (down (char= (char str 0) #\D))
	  (right (char= (char str 0) #\R))
	  (num (read-from-string (subseq str 1))))
      (cond
	(left `(left ,num))
	(right `(right ,num))
	(up `(up ,num))
	(down `(down ,num))
	(t (error "bad direction"))))))


(defun convert (xs)
  (mapcar #'cnv xs))

;; R right
;; D down
;; L left
;; U up
;; two wires on input
(defparameter wire1 (convert (car input)))
(defparameter wire2 (convert (car (cdr input))))

;; have a method uses lambda x y returns true if at x y or false if not
;; say at 0,0
;; have right N
;; x,y to x+N,y are inclusive , continue with x+N,y
;;
;; have left N
;; x,y  x-N,y
;;
;; have up N
;; x,y  x,y-N
;;
;; have down N
;; x,y ... x,y+N inclusive range

(defmacro horz-between (a b c d)
  `(let ((lo (min ,a ,c))
	 (hi (max ,a ,c)))
     (and (= y ,d) (>= x lo) (<= x hi))))

(defmacro vert-between (a b c d)
  `(let ((lo (min ,b ,d))
	 (hi (max ,b ,d)))
     (and (= x ,a) (>= y lo) (<= y hi))))


(defun wire-helper (x y wire)
  (cond
    ((null wire) wire)
    (t (let* ((expr (car wire))
	      (dir (car expr))
	      (dist (car (cdr expr))))
	 (cond
	   ((eq dir 'right) (cons `(horz-between ,x ,y ,(+ x dist) ,y)
				   (wire-helper (+ x dist) y (cdr wire))))
	   ((eq dir 'left) (cons `(horz-between ,x ,y ,(- x dist) ,y)
				  (wire-helper (- x dist) y (cdr wire))))
	   ((eq dir 'up) (cons `(vert-between ,x ,y ,x ,(- y dist))
				(wire-helper x (- y dist) (cdr wire))))
	   ((eq dir 'down) (cons `(vert-between ,x ,y ,x ,(+ y dist))
				  (wire-helper x (+ y dist) (cdr wire))))
	   (t (error "bad-dir-wire-helper")))))))

(defun active1 (x y)
  (cons 'or (wire-helper x y wire1)))

(defun active2 (x y)
  (cons 'or (wire-helper x y wire2)))


(defmacro on-wire1 ()
  `(defun mywire1 (x y) ,(active1 0 0)))

(defmacro on-wire2 ()
  `(defun mywire2 (x y) ,(active2 0 0)))

;; define mywire1 , mywire2
(on-wire1)
(on-wire2)

;; brute force search
(defun brute (dist)
  (let ((pos dist)
	(neg (- dist))
	(solutions nil))
    (format t "brute force distance -~a to +~a ~%" neg pos)
    (loop for x from neg to pos do
      (loop for y from neg to pos do
	(when (and (mywire1 x y) (mywire2 x y))
	  (setq solutions (cons (list (+ (abs x) (abs y)) (list x y)) solutions))
	  (format t "found intersection at ~a ~a ~%" x y))))
    (sort solutions (lambda (x y)(< (car x) (car y))))))



;; (brute 3000)
;; 2 * 3000 * 3000 procedure calls to check if X,Y
;; is both on (mywire1 x y) AND (mywire2 x y)
;;
;; bit hard to see as mywire1 , mywire2 are built at runtime 
;;
#|
AOC> (time (brute 3000))
brute force distance --3000 to +3000 
found intersection at -2366 1571 
found intersection at -2327 1571 
found intersection at -1745 1182 
found intersection at -1745 1245 
found intersection at -1579 1182 
found intersection at -1579 1245 
found intersection at -1553 1355 
found intersection at -1553 1480 
found intersection at -1553 1571 
found intersection at -1520 1245 
found intersection at -1387 1355 
found intersection at -1387 1480 
found intersection at -609 1166 
found intersection at -446 301 
found intersection at -437 1257 
found intersection at -415 526 
found intersection at -161 1257 
found intersection at -90 861 
found intersection at -90 865 
found intersection at -90 1140 
found intersection at -3 526 
found intersection at -3 572 
found intersection at 0 0 
found intersection at 36 861 
found intersection at 36 865 
found intersection at 36 1140 
found intersection at 81 1460 
found intersection at 956 -355 
found intersection at 993 -355 
found intersection at 1325 -184 
found intersection at 1325 -132 
found intersection at 1335 -847 
found intersection at 1335 -814 
found intersection at 1340 -10 
found intersection at 1483 -10 
found intersection at 1535 -847 
found intersection at 1861 -817 
found intersection at 1889 -684 
found intersection at 1889 -635 
found intersection at 1889 -531 
found intersection at 2019 -284 
found intersection at 2019 -10 
found intersection at 2019 141 
found intersection at 2125 -684 
found intersection at 2125 -531 
found intersection at 2152 -684 
found intersection at 2152 -531 
found intersection at 2169 -284 
found intersection at 2169 -230 
found intersection at 2191 -29 
found intersection at 2440 -230 
found intersection at 2817 -204 
Evaluation took:
  13.684 seconds of real time
  13.691749 seconds of total run time (13.687542 user, 0.004207 system)
  100.06% CPU
  50,449,788,246 processor cycles
  163,904 bytes consed
  
((0 (0 0)) (529 (-3 526)) (575 (-3 572)) (747 (-446 301)) (897 (36 861))
 (901 (36 865)) (941 (-415 526)) (951 (-90 861)) (955 (-90 865))
 (1176 (36 1140)) (1230 (-90 1140)) (1311 (956 -355)) (1348 (993 -355))
 (1350 (1340 -10)) (1418 (-161 1257)) (1457 (1325 -132)) (1493 (1483 -10))
 (1509 (1325 -184)) (1541 (81 1460)) (1694 (-437 1257)) (1775 (-609 1166))
 (2029 (2019 -10)) (2149 (1335 -814)) (2160 (2019 141)) (2182 (1335 -847))
 (2220 (2191 -29)) (2303 (2019 -284)) (2382 (1535 -847)) (2399 (2169 -230))
 (2420 (1889 -531)) (2453 (2169 -284)) (2524 (1889 -635)) (2573 (1889 -684))
 (2656 (2125 -531)) (2670 (2440 -230)) (2678 (1861 -817)) (2683 (2152 -531))
 (2742 (-1387 1355)) (2761 (-1579 1182)) (2765 (-1520 1245)) (2809 (2125 -684))
 (2824 (-1579 1245)) (2836 (2152 -684)) (2867 (-1387 1480)) (2908 (-1553 1355))
 (2927 (-1745 1182)) (2990 (-1745 1245)) (3021 (2817 -204)) (3033 (-1553 1480))
 (3124 (-1553 1571)) (3898 (-2327 1571)) (3937 (-2366 1571)))

AOC>

(529 (-3 526))
 ^--- solution manhattan distance
 with intersection at x= -3 ; y= 526



|#







	   
	   
	   
	   
	 
	   




