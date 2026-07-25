
(defpackage :fun
  (:use :cl))

(in-package :fun)

(defstruct moon
  name
  position 
  velocity)

;; deep copy moon ?


(defun input ()
  (let ((moon1 nil)
	(moon2 nil)
	(moon3 nil)
	(moon4 nil))
    (setq moon1 (make-moon :name 'a :position #(4 1 1) :velocity #(0 0 0)))
    (setq moon2 (make-moon :name 'b :position #(11 -18 -1) :velocity #(0 0 0)))
    (setq moon3 (make-moon :name 'c :position #(-2 -10 -4) :velocity #(0 0 0)))
    (setq moon4 (make-moon :name 'd :position #(-7 -2 14) :velocity #(0 0 0)))
    (list moon1 moon2 moon3 moon4)))


(defun example ()
  (let ((moon1 nil)
	(moon2 nil)
	(moon3 nil)
	(moon4 nil))
    (setq moon1 (make-moon :name 'a :position #(-1 0 2) :velocity #(0 0 0)))
    (setq moon2 (make-moon :name 'b :position #(2 -10 -7) :velocity #(0 0 0)))
    (setq moon3 (make-moon :name 'c :position #(4 -8 8) :velocity #(0 0 0)))
    (setq moon4 (make-moon :name 'd :position #(3 5 -1) :velocity #(0 0 0)))
    (list moon1 moon2 moon3 moon4)))

;; <x=-8, y=-10, z=0>
;; <x=5, y=5, z=10>
;; <x=2, y=-7, z=3>
;; <x=9, y=-8, z=-3>

(defun example2 ()
  (let ((moon1 nil)
	(moon2 nil)
	(moon3 nil)
	(moon4 nil))
    (setq moon1 (make-moon :name 'a :position #(-8 -10 0) :velocity #(0 0 0)))
    (setq moon2 (make-moon :name 'b :position #(5 5 10) :velocity #(0 0 0)))
    (setq moon3 (make-moon :name 'c :position #(2 -7 3) :velocity #(0 0 0)))
    (setq moon4 (make-moon :name 'd :position #(9 -8 -3) :velocity #(0 0 0)))
    (list moon1 moon2 moon3 moon4)))


(defun px(p)
  (assert (eq 'moon (type-of p)))
  (aref (moon-position p) 0))

(defun py(p)
  (assert (eq 'moon (type-of p)))
  (aref (moon-position p) 1))

(defun pz(p)
  (assert (eq 'moon (type-of p)))
  (aref (moon-position p) 2))

(defun vx(p)
  (assert (eq 'moon (type-of p)))
  (aref (moon-velocity p) 0))

(defun vy(p)
  (assert (eq 'moon (type-of p)))
  (aref (moon-velocity p) 1))

(defun vz(p)
  (assert (eq 'moon (type-of p)))
  (aref (moon-velocity p) 2))


(defun incr-vx(p)
  (assert (eq 'moon (type-of p)))
  (incf (aref (moon-velocity p) 0)))

(defun incr-vy(p)
  (assert (eq 'moon (type-of p)))
  (incf (aref (moon-velocity p) 1)))

(defun incr-vz(p)
  (assert (eq 'moon (type-of p)))
  (incf (aref (moon-velocity p) 2)))

(defun decr-vx(p)
  (assert (eq 'moon (type-of p)))
  (decf (aref (moon-velocity p) 0)))

(defun decr-vy(p)
  (assert (eq 'moon (type-of p)))
  (decf (aref (moon-velocity p) 1)))

(defun decr-vz(p)
  (assert (eq 'moon (type-of p)))
  (decf (aref (moon-velocity p) 2)))



(defmacro change-velocity(axis up down moon1 moon2)
  `(progn
     (when (< (,axis ,moon1)(,axis ,moon2))
       (,up ,moon1)
       (,down ,moon2))
     (when (< (,axis ,moon2)(,axis ,moon1))
       (,up ,moon2)
       (,down ,moon1))))
     
    
(defun apply-gravity(moons)
  (let ((smoons moons))
    (loop while (not (null (cdr smoons))) do
      (let ((moon1 (car smoons)))
	(loop for moon2 in (cdr smoons) do
	  ;; change velocity based on moon positions
	  (change-velocity px incr-vx decr-vx moon1 moon2)
	  (change-velocity py incr-vy decr-vy moon1 moon2)
	  (change-velocity pz incr-vz decr-vz moon1 moon2)
	  ;; (format t "moon ~a with moon ~a~%" (moon-name moon1) (moon-name moon2))
	      )
	(setq smoons (cdr smoons)))))
  moons)

(defun apply-velocity(moons)
  (loop for moon in moons do
    (loop for i from 0 to 2 do 
      (setf (aref (moon-position moon) i)
	    (+ (aref (moon-position moon) i)
	       (aref (moon-velocity moon) i)))))
  moons)

(defun one-step(moons)
  (apply-gravity moons)
  (apply-velocity moons))

(defun potential-energy (moon)
  (+ (abs (px moon))
     (abs (py moon))
     (abs (pz moon))))

(defun kinetic-energy (moon)
  (+ (abs (vx moon))
     (abs (vy moon))
     (abs (vz moon))))

(defun total-energy (moon)
  (* (potential-energy moon)
     (kinetic-energy moon)))

(defun run-for(moons n-steps)
  (show-moons 0 moons)
  (loop for i from 1 to n-steps do
    (when (zerop (mod i 10))   (show-moons i moons))
    (one-step moons))
  (let ((total 0))
    (loop for moon in moons do
      (incf total (total-energy moon)))
    total))


(defun show-moons(step moons)
  (format t "step ~a~%" step)
  (loop for moon in moons do
    (format t "~a~%" moon)))


(defun trial1()
  (run-for (example) 10))

(defun trial2()
  (run-for (example2) 100))

(defun part1 ()
  (run-for (input) 1000))
;; 9493

;; can we over-ride default moon copy 
(defun copy-moon (moon)
  (let ((tmp (make-moon :name (moon-name moon)
			:position (make-array 3)
			:velocity (make-array 3))))
    (setf (aref (moon-position tmp) 0) (px moon))
    (setf (aref (moon-position tmp) 1) (py moon))
    (setf (aref (moon-position tmp) 2) (pz moon))
    (setf (aref (moon-velocity tmp) 0) (vx moon))
    (setf (aref (moon-velocity tmp) 1) (vy moon))
    (setf (aref (moon-velocity tmp) 2) (vz moon))
    tmp))
    	     

;; can we copy structure ??
(defun test2 ()
  (let* ((moons (list (make-moon :name 'a :position #(4 1 1) :velocity #(0 0 0))
		      (make-moon :name 'b :position #(11 -18 -1) :velocity #(0 0 0))
		      (make-moon :name 'c :position #(-2 -10 -4) :velocity #(0 0 0))
		      (make-moon :name 'd :position #(-7 -2 14) :velocity #(0 0 0))))
	 (moons2 (mapcar #'copy-moon moons)))
    (run-for moons2 100)
    moons))

(defun test3 ()
  (let* ((moons (example))
	 (original (mapcar #'copy-moon moons))
	 (count 0)
	 (keep-going t))
    (loop while keep-going do
      (one-step moons)
      (incf count)
      (when (equalp moons original)
	(format t "repeat at index ~a~%" count)
	(setq keep-going nil)))))

(defun test4 ()
  (let* ((moons (example2))
	 (original (mapcar #'copy-moon moons))
	 (count 0)
	 (keep-going t))
    (loop while keep-going do
      (one-step moons)
      (incf count)
      (when (zerop (mod count (expt 10 6)))
	(format t "progress ~a~%" count))
      (when (equalp moons original)
	(format t "repeat at index ~a~%" count)
	(setq keep-going nil)))))

      
  

