module Main where

import Prelude hiding (lookup)
 
import Data.Sequence 

-- import Data.Foldable 
--- F.toList
--- S.fromList

--- :set +t
--- [x] means list of x
--- :: means has type of
--- : means cons - List constructor
--- [1,2,3] :: [Int]

fred = fromList [1,2,3,4,5]

--- 

input = fromList ([999,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0] :: [Integer])

add v j k z = let n1 = lookup j v
                  n2 = lookup k v
              in case (n1 ,n2) of
                   (Just i1, Just i2) -> Just (update z (i1 + i2) v)
                   _ -> Nothing

mul v j k z = let n1 = lookup j v
                  n2 = lookup k v
              in case (n1 ,n2) of
                   (Just i1, Just i2) -> Just (update z (i1 * i2) v)
                   _ -> Nothing

--- lookup from Data.Sequence
--- update 

-- length defined on sequeance ok

-- zeroth element from Data.Seq 

-- ;; 1 add
-- ;; 2 mul 
-- ;; or 99 halt

-- add s v j k z 

-- (defn add [v j k z]
--   (assoc v z (+ (nth v j) (nth v k))))
-- ;;(add [1 2 0] 0 1 2)

-- (defn mul [v j k z]
--   (assoc v z (* (nth v j) (nth v k))))
-- ;;(mul [1 2 0] 0 1 2)

sim v i = let op = lookup i v
              j = lookup (i + 1) v
              k = lookup (i + 2) v
              z = lookup (i + 3) v
          in case (op,j,k,z) of
               (1,Just a,Just b, Just c) -> let ov = add v j k z
                                            in case ov of
                                               Just v2 -> sim v2 (i + 4)
                                               Nothing -> Nothing
               (2,Just a,Just b, Just c) -> let ov = mul v j k z
                                            in case ov of
                                               Just v2 -> Just (sim v2 (i + 4))
                                               Nothing -> Nothing
               (99) -> Just v


                       
                                               



-- (defn sim [v2]
--   (loop [v v2
--          i 0]
--     (let [op (nth v i)]
--       (cond
--         (= op 1) (let [j (nth v (+ i 1))
--                        k (nth v (+ i 2))
--                        z (nth v (+ i 3))]
--                    (recur (add v j k z) (+ i 4)))
--         (= op 2) (let [j (nth v (+ i 1))
--                        k (nth v (+ i 2))
--                        z (nth v (+ i 3))]
--                    (recur (mul v j k z) (+ i 4)))
--         (= op 99) v
--         true (throw (Exception. "opcode not 1 2 or 99"))))))


-- ;; part 1

-- (defn part-1 []
--   (let [v (assoc in 1 12)
--         v (assoc v 2 2)]
--     (nth (sim v) 0)))

-- ;; using global 'in' vector 
-- (defn try-2 [v noun verb]
--   (let [v2 (assoc v 1 noun)
--         v3 (assoc v2 2 verb)]
--     (nth (sim v3) 0)))



-- ;; part 2 
-- ;; put guards at start of loop - ensure cannot get spurious values
-- (defn part-2 []
--   (loop [n 0
--          v 0]
--     (cond
--       (> v 99) nil
--       (> n 99) (recur 0 (+ v 1))
--       true (do
--              (print (format "noun %d , verb %d\n" n v))
--              (let [out (try-2 in n v)]
--                #break "#break did nothing..."
--                (cond
--                  (= 19690720 out) (+ (* 100 n) v)
--                  true (recur (+ n 1) v)))))))


main :: IO ()
main = putStrLn "Hello, Haskell!"

