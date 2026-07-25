
(defpackage :gen
  (:use :cl))

(in-package :gen)

(defun types () 
  (format t "// types ~%")
  (format t "static int64_t count = 0LL ;~%")
  (loop for i from 1 to 4 do   
    (format t "static int64_t px~a = 0LL ;~%" i)
    (format t "static int64_t py~a = 0LL ;~%" i)
    (format t "static int64_t pz~a = 0LL ;~%" i)
    (format t "static int64_t vx~a = 0LL ;~%" i)
    (format t "static int64_t vy~a = 0LL ;~%" i)
    (format t "static int64_t vz~a = 0LL ;~%" i))
  (format t "~%"))

(defparameter px1 4)
(defparameter py1 0)
(defparameter pz1 0)

(defparameter px2 4)
(defparameter py2 0)
(defparameter pz2 0)

(defparameter px3 4)
(defparameter py3 0)
(defparameter pz3 0)

(defparameter px4 4)
(defparameter py4 0)
(defparameter pz4 0)






  
  

;; c code generator
(defun apply-gravity (n1 n2)
  (format t "// moons ~a - ~a ~%" n1 n2)
  (format t "// x ~%")
  (loop for axis in '("x" "y" "z") do
    (format t "// ~a axis ~%" axis)
    (format t "if (p~a~a < p~a~a) { v~a~a++; v~a~a--; } ~%"
	    axis n1
	    axis n2
	    axis n1
	    axis n2)
    (format t "if (p~a~a < p~a~a) { v~a~a++; v~a~a--; } ~%"
	    axis n2
	    axis n1
	    axis n2
	    axis n1)
	))

(defun apply-velocity ()
  (loop for i from 1 to 4 do 
    (loop for axis in '("x" "y" "z") do  
      (format t "p~a~a = p~a~a + " axis i axis i)
      (format t "v~a~a;~%" axis i)
      )))

(defun check-no-change ()
  (format t "count++; // steps we have computed ~%")
  (format t "if (count % 1000000 == 0) { ~%")
  (format t "printf(\"explored %\" PRId64 \"\\n\",count);~%")  
  (format t "}~%")
  
  
  (format t "if (px1 != ~aLL) { continue; }~%" px1)
  (format t "if (py1 != ~aLL) { continue; }~%" py1)
  (format t "if (pz1 != ~aLL) { continue; }~%" pz1)

  (format t "if (px2 != ~aLL) { continue; }~%" px2)
  (format t "if (py2 != ~aLL) { continue; }~%" py2)
  (format t "if (pz2 != ~aLL) { continue; }~%" pz2)

  (format t "if (px3 != ~aLL) { continue; }~%" px3)
  (format t "if (py3 != ~aLL) { continue; }~%" py3)
  (format t "if (pz3 != ~aLL) { continue; }~%" pz3)

  (format t "if (px4 != ~aLL) { continue; }~%" px4)
  (format t "if (py4 != ~aLL) { continue; }~%" py4)
  (format t "if (pz4 != ~aLL) { continue; }~%" pz4)
  
  (format t "break;~%")
  
  )

;; <x=4, y=1, z=1>
;; <x=11, y=-18, z=-1>
;; <x=-2, y=-10, z=-4>
;; <x=-7, y=-2, z=14>

(defun init ()
  (format t "px1 = ~aLL ; py1 = ~aLL ; pz1 = ~aLL ;~%" px1 py1 pz1)
  (format t "px2 = ~aLL ; py2 = ~aLL ; pz2 = ~aLL ;~%" px2 py2 pz2)
  (format t "px3 = ~aLL ; py3 = ~aLL ; pz3 = ~aLL ;~%" px3 py3 pz3)
  (format t "px4 = ~aLL ; py4 = ~aLL ; pz4 = ~aLL ;~%" px4 py4 pz4)
  
  )

(defun headers ()
  (format t "#include <stdio.h>~%")
  (format t "#include <stdlib.h>~%")
  (format t "#include <string.h>~%")
  (format t "#include <inttypes.h>~%")
  )

(defun main-entry ()
  (format t "int main(){~%")  
  )

(defun main-exit ()
  (format t "return 0;~%")
  (format t "}~%"))


(defun while-entry ()
  (format t "while (1) {~%")  
  )

(defun while-exit ()
  (format t "~%")
  (format t "}~%"))

(defun results ()
  (format t "count++;~%");
  (format t "printf(\"met itself %\" PRId64 \"\\n \", count);~%");
  )


(defun gen ()

  (headers)
  
  (types)

  (main-entry)

  (init)
  
  (while-entry)
  
  (apply-gravity 1 2)
  (apply-gravity 1 3)
  (apply-gravity 1 4)
  (apply-gravity 2 3)
  (apply-gravity 2 4)
  (apply-gravity 3 4)

  (apply-velocity)

  (check-no-change)
  (while-exit)

  (results)
  
  (main-exit)
  
  )

;; <x=-1, y=0, z=2>
;; <x=2, y=-10, z=-7>
;; <x=4, y=-8, z=8>
;; <x=3, y=5, z=-1>
(defun example1 ()
  (let ((px1 -1)(py1 0)(pz1 2)
		(px2 2)(py2 -10)(pz2 -7)
		(px3 4)(py3 -8)(pz3 8)
		(px4 3)(py4 5)(pz4 -1))
    (gen)))


;; <x=-8, y=-10, z=0>
;; <x=5, y=5, z=10>
;; <x=2, y=-7, z=3>
;; <x=9, y=-8, z=-3>
(defun example2 ()
  (let ((px1 -8)(py1 -10)(pz1 0)
		(px2 5)(py2 5)(pz2 10)
		(px3 2)(py3 -7)(pz3 3)
		(px4 9)(py4 -8)(pz4 -3))
    (gen)))



;; <x=4, y=1, z=1>
;; <x=11, y=-18, z=-1>
;; <x=-2, y=-10, z=-4>
;; <x=-7, y=-2, z=14>
(defun part2 ()
  (let ((px1 4)(py1 1)(pz1 1)
		(px2 11)(py2 -18)(pz2 -1)
		(px3 -2)(py3 -10)(pz3 -4)
		(px4 -7)(py4 -2)(pz4 14))
    (gen)))





;; (defmacro change-velocity(axis up down moon1 moon2)
;;   `(progn
;;      (when (< (,axis ,moon1)(,axis ,moon2))
;;        (,up ,moon1)
;;        (,down ,moon2))
;;      (when (< (,axis ,moon2)(,axis ,moon1))
;;        (,up ,moon2)
;;        (,down ,moon1))))
     
    
;; (defun apply-gravity(moons)
;;   (let ((smoons moons))
;;     (loop while (not (null (cdr smoons))) do
;;       (let ((moon1 (car smoons)))
;; 	(loop for moon2 in (cdr smoons) do
;; 	  ;; change velocity based on moon positions
;; 	  (change-velocity px incr-vx decr-vx moon1 moon2)
;; 	  (change-velocity py incr-vy decr-vy moon1 moon2)
;; 	  (change-velocity pz incr-vz decr-vz moon1 moon2)
;; 	  ;; (format t "moon ~a with moon ~a~%" (moon-name moon1) (moon-name moon2))
;; 	      )
;; 	(setq smoons (cdr smoons)))))
;;   moons)

;; (defun apply-velocity(moons)
;;   (loop for moon in moons do
;;     (loop for i from 0 to 2 do 
;;       (setf (aref (moon-position moon) i)
;; 	    (+ (aref (moon-position moon) i)
;; 	       (aref (moon-velocity moon) i)))))
;;   moons)

;; (defun one-step(moons)
;;   (apply-gravity moons)
;;   (apply-velocity moons))

;; (defun potential-energy (moon)
;;   (+ (abs (px moon))
;;      (abs (py moon))
;;      (abs (pz moon))))

;; (defun kinetic-energy (moon)
;;   (+ (abs (vx moon))
;;      (abs (vy moon))
;;      (abs (vz moon))))

;; (defun total-energy (moon)
;;   (* (potential-energy moon)
;;      (kinetic-energy moon)))

;; (defun run-for(moons n-steps)
;;   (show-moons 0 moons)
;;   (loop for i from 1 to n-steps do
;;     (when (zerop (mod i 10))   (show-moons i moons))
;;     (one-step moons))
;;   (let ((total 0))
;;     (loop for moon in moons do
;;       (incf total (total-energy moon)))
;;     total))


;; (defun show-moons(step moons)
;;   (format t "step ~a~%" step)
;;   (loop for moon in moons do
;;     (format t "~a~%" moon)))


;; (defun trial1()
;;   (run-for (example) 10))

;; (defun trial2()
;;   (run-for (example2) 100))

;; (defun part1 ()
;;   (run-for (input) 1000))
;; ;; 9493

;; ;; can we over-ride default moon copy 
;; (defun copy-moon (moon)
;;   (let ((tmp (make-moon :name (moon-name moon)
;; 			:position (make-array 3)
;; 			:velocity (make-array 3))))
;;     (setf (aref (moon-position tmp) 0) (px moon))
;;     (setf (aref (moon-position tmp) 1) (py moon))
;;     (setf (aref (moon-position tmp) 2) (pz moon))
;;     (setf (aref (moon-velocity tmp) 0) (vx moon))
;;     (setf (aref (moon-velocity tmp) 1) (vy moon))
;;     (setf (aref (moon-velocity tmp) 2) (vz moon))
;;     tmp))
    	     

;; ;; can we copy structure ??
;; (defun test2 ()
;;   (let* ((moons (list (make-moon :name 'a :position #(4 1 1) :velocity #(0 0 0))
;; 		      (make-moon :name 'b :position #(11 -18 -1) :velocity #(0 0 0))
;; 		      (make-moon :name 'c :position #(-2 -10 -4) :velocity #(0 0 0))
;; 		      (make-moon :name 'd :position #(-7 -2 14) :velocity #(0 0 0))))
;; 	 (moons2 (mapcar #'copy-moon moons)))
;;     (run-for moons2 100)
;;     moons))

;; (defun test3 ()
;;   (let* ((moons (example))
;; 	 (original (mapcar #'copy-moon moons))
;; 	 (count 0)
;; 	 (keep-going t))
;;     (loop while keep-going do
;;       (one-step moons)
;;       (incf count)
;;       (when (equalp moons original)
;; 	(format t "repeat at index ~a~%" count)
;; 	(setq keep-going nil)))))

;; (defun test4 ()
;;   (let* ((moons (example2))
;; 	 (original (mapcar #'copy-moon moons))
;; 	 (count 0)
;; 	 (keep-going t))
;;     (loop while keep-going do
;;       (one-step moons)
;;       (incf count)
;;       (when (zerop (mod count (expt 10 6)))
;; 	(format t "progress ~a~%" count))
;;       (when (equalp moons original)
;; 	(format t "repeat at index ~a~%" count)
;; 	(setq keep-going nil)))))

      
  

