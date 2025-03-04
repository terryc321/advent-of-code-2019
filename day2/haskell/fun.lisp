
(defpackage :aoc
  (:use :cl))
(in-package :aoc)

(defparameter input
  (with-open-file (stream "../input" :direction :input)
    (read stream)))

(defun put (xs tar v)
  (labels ((put2 (xs n tar v)
	     (cond
	       ((null xs) xs)
	       ((= n tar) (cons v (cdr xs)))
	       (t (cons (car xs) (put2 (cdr xs) (+ n 1) tar v))))))
    (put2 xs 0 tar v)))



(defun run (code)
  (let ((p 0)
	(len (length code)))
    (catch 'code
      (loop while t do
	(let* ((op (nth (+ p 0) code)))
	  (when (= op 99) (throw 'code code))
	  (let*((op1 (nth (nth (+ p 1) code) code))
		(op2 (nth (nth (+ p 2) code) code))
		(tar (nth (+ p 3) code)))
	    (cond
	      ((= op 1)
	       (format t "op ADD : op1 = ~a : op2 = ~a ~%" op1 op2)
	       (setq code (put code tar (+ op1 op2))))
	      ((= op 2)
	       (format t "op MUL : op1 = ~a : op2 = ~a ~%" op1 op2)
	       (setq code (put code tar (* op1 op2))))
	      ((= op 99) (throw 'code code))
	      (t (error "wtf")))	  	
	    (setq p (+ p 4))))))))




      
(defun setup ()
  (let* ((code input)
	 (n1 (put code 1 12))
	 (n2 (put n1 2 2)))
    (run n2)))


#|
(run '(1 0 0 0 99))            2,0,0,0,99
(run '(2 3 0 3 99))            2,3,0,6,99
(run '(2 4 4 5 99 0))          2,4,4,5,99,9801
(run '(1 1 1 4 99 5 6 0 99))   30,1,1,4,2,5,6,0,99
becomes 2 0 0 0 99 (1 + 1 = 2).
2 3 0 3 99 becomes 2 3 0 6 99 (3 * 2 = 6).
2 4 4 5 99 0 becomes 2 4 4 5 99 9801 (99 * 99 = 9801).
1 1 1 4 99 5 6 0 99 becomes 30 1 1 4 2 5 6 0 99.
|#

  

;; import System.IO  
;; -- import Control.Monad

;; rev = reverse

;; -- tokenise contents of file into a sequence of number strings only
;; -- ignore newlines
;; -- ignore parenthesis
;; tok2 [] [] rs = rev rs
;; tok2 [] c rs = rev ((rev c) : rs)
;; tok2 (h:t) [] rs = if isDigit h then tok2 t ([h]) rs else tok2 t [] rs
;; tok2 (h:t) c rs = if isDigit h then tok2 t (h:c) rs else tok2 t [] ((rev c) : rs)
;; tok xs = tok2 xs [] []



;; -- split a string into multiple strings 
;; split2 [] [] rs = rev rs
;; split2 [] c rs = rev ((rev c) : rs)
;; split2 (h:t) [] rs = if h == '\n' then split2 t [] rs
;;                     else split2 t ([h]) rs
;; split2 (h:t) c rs = if h == '\n' then split2 t [] ((rev c) : rs)
;;                     else split2 t (h:c) rs
;; split xs = split2 xs [] []

;; --- all characters 0 to 9 inclusive
;; isDigit '0' = True
;; isDigit '1' = True
;; isDigit '2' = True
;; isDigit '3' = True
;; isDigit '4' = True
;; isDigit '5' = True
;; isDigit '6' = True
;; isDigit '7' = True
;; isDigit '8' = True
;; isDigit '9' = True
;; isDigit _ = False

;; -- is list all digits ?
;; allDigits [] = True
;; allDigits (h:t) = if isDigit h then allDigits t else False

;; -- filter any strings not allDigits

;; -- convert string integer to an integer
;; s2num2 [] n = n
;; s2num2 ('0':t) n = s2num2 t (n*10 + 0)
;; s2num2 ('1':t) n = s2num2 t (n*10 + 1)
;; s2num2 ('2':t) n = s2num2 t (n*10 + 2)
;; s2num2 ('3':t) n = s2num2 t (n*10 + 3)
;; s2num2 ('4':t) n = s2num2 t (n*10 + 4)
;; s2num2 ('5':t) n = s2num2 t (n*10 + 5)
;; s2num2 ('6':t) n = s2num2 t (n*10 + 6)
;; s2num2 ('7':t) n = s2num2 t (n*10 + 7)
;; s2num2 ('8':t) n = s2num2 t (n*10 + 8)
;; s2num2 ('9':t) n = s2num2 t (n*10 + 9)
;; s2num s = s2num2 s 0

;; -- replace item N with x if possible
;; put2 [] n tar x = []
;; put2 (h:t) n tar x = if n == tar then x : t
;;   else h : (put2 t (n+1) tar x)
;; put code tar x = put2 code 0 tar x

;; -- check that put changes one item in list at nth starting at zero ...
;; -- does not tell us if we do not make substitution tho.
;; p0 = put [0,1,2,3] 0 7
;; p1 = put [0,1,2,3] 1 7
;; p2 = put [0,1,2,3] 2 7
;; p3 = put [0,1,2,3] 3 7
;; p4 = put [0,1,2,3] 4 7




;; -- haskell !! operator gives nth element of list zeroth 0 is first element
;; -- position p
;; -- op !! p
;; -- op1 !! p + 1
;; -- op2 !! p + 2
;; -- tar !! p + 3

;; run2 code p len =
;;   if p < 0 then code 
;;   else if p >= len then code
;;   else let op = code !! p
;;            op1 = code !! (p + 1)
;;            op2 = code !! (p + 2)
;;            tar = code !! (p + 3)
;;        in
;;          if op == 1 then
;;            run2 (put code tar (op1 + op2)) (p + 4) len
;;          else if op == 2 then
;;                 run2 (put code tar (op1 * op2)) (p + 4) len           
;;          else if op == 99 then
;;            code
;;          else 
;;               code


;; run code = run2 code 0 (length code)


;; main :: IO ()
;; main = do  
;;         let list = []
;;         handle <- openFile "../input" ReadMode
;;         contents <- hGetContents handle
;;         print contents
;;         --print (tok contents)
;;         let nums = map s2num (tok contents)
;;         print nums
        
;;         let n1 = (put nums 1 12) in
;;           let n2 = (put n1 2 2) in
;;             do print n2
;;                let n3 = run n2 in
;;                  print (n3 !! 0)
                
           
               
                 

               

        
;;         -- let masses = map mass nums
;;         -- print ("masses=",masses)
;;         -- let total = foldr (+) 0 masses 
;;         -- print ("total",total)
;;         hClose handle
        


;; -- f :: [String] -> [Int]
;; -- f = map read


