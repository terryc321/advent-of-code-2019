(ns clojure-noob.core
  (:gen-class))

;; created with
;; > lein new app clojure-noob
;;

(def in
  "some description"
  [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0])

;; (type in) 
;; (count in) 
;; (doc count)
;; (doc type)
;; (nth coll index)
;;
;; change 132nd element of in vector with 1 by using ASSOC
;; (assoc in 132 1)
;; (doc assoc)
;; if we ASSOC on COUNT of vector - we end up growing the VECTOR by 1 item === a push !!!
;;
;; 1 add
;; 2 mul 
;; or 99 halt

;; pr
;; print
;; println
;;

(defn add [v j k z]
  (assoc v z (+ (nth v j) (nth v k))))
;;(add [1 2 0] 0 1 2)

(defn mul [v j k z]
  (assoc v z (* (nth v j) (nth v k))))
;;(mul [1 2 0] 0 1 2)


(defn sim [v2]
  (loop [v v2
         i 0]
    (let [op (nth v i)]
      (cond
        (= op 1) (let [j (nth v (+ i 1))
                       k (nth v (+ i 2))
                       z (nth v (+ i 3))]
                   (recur (add v j k z) (+ i 4)))
        (= op 2) (let [j (nth v (+ i 1))
                       k (nth v (+ i 2))
                       z (nth v (+ i 3))]
                   (recur (mul v j k z) (+ i 4)))
        (= op 99) v
        true (throw (Exception. "opcode not 1 2 or 99"))))))


;; part 1

(defn part-1 []
  (let [v (assoc in 1 12)
        v (assoc v 2 2)]
    (nth (sim v) 0)))

;; using global 'in' vector 
(defn try-2 [v noun verb]
  (let [v2 (assoc v 1 noun)
        v3 (assoc v2 2 verb)]
    (nth (sim v3) 0)))



;; part 2 
;; put guards at start of loop - ensure cannot get spurious values
(defn part-2 []
  (loop [n 0
         v 0]
    (cond
      (> v 99) nil
      (> n 99) (recur 0 (+ v 1))
      true (do
             (print (format "noun %d , verb %d\n" n v))
             (let [out (try-2 in n v)]
               #break "#break did nothing..."
               (cond
                 (= 19690720 out) (+ (* 100 n) v)
                 true (recur (+ n 1) v)))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


