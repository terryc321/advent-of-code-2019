

-- focus on pure core working rather than parsing a file

prog1 :: [Int] = [1,9,10,3,2,3,11,0,99,30,40,50]
prog2 :: [Int] = [1,0,0,0,99]
prog3 :: [Int] = [2,3,0,3,99]
prog4 :: [Int] = [2,4,4,5,99,0]
prog5 :: [Int] = [1,1,1,4,99,5,6,0,99] 
progX :: [Int] = [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 13, 1, 19, 1, 19, 10, 23, 2, 10, 23, 27, 1, 27, 6, 31, 1, 13, 31, 35, 1, 13, 35, 39, 1, 39, 10, 43, 2, 43, 13, 47, 1, 47, 9, 51, 2, 51, 13, 55, 1, 5, 55, 59, 2, 59, 9, 63, 1, 13, 63, 67, 2, 13, 67, 71, 1, 71, 5, 75, 2, 75, 13, 79, 1, 79, 6, 83, 1, 83, 5, 87, 2, 87, 6, 91, 1, 5, 91, 95, 1, 95, 13, 99, 2, 99, 6, 103, 1, 5, 103, 107, 1, 107, 9, 111, 2, 6, 111, 115, 1, 5, 115, 119, 1, 119, 2, 123, 1, 6, 123, 0, 99, 2, 14, 0, 0]

-- haskell !! operator gives nth element of list zeroth 0 is first element
-- position p
-- op !! p
-- op1 !! p + 1
-- op2 !! p + 2
-- tar !! p + 3

-- replace item N with x if possible
put2 [] n tar x = []
put2 (h:t) n tar x = if n == tar then x : t
  else h : (put2 t (n+1) tar x)
put code tar x = put2 code 0 tar x

-- check that put changes one item in list at nth starting at zero ...
-- does not tell us if we do not make substitution tho.
p0 = put [0,1,2,3] 0 7
p1 = put [0,1,2,3] 1 7
p2 = put [0,1,2,3] 2 7
p3 = put [0,1,2,3] 3 7
p4 = put [0,1,2,3] 4 7

op_halt :: Int = 99
op_add :: Int = 1
op_mul :: Int = 2

run2 c p l =
  if p < 0 then c 
  else if p >= l then c
  else let op = c !! p           
       in
    if op == op_halt then c -- done
    else let op1 = c !! (c !! (p + 1))
             op2 = c !! (c !! (p + 2))
             tar = c !! (p + 3)
         in
      if op == op_add then
        run2 (put c tar (op1 + op2)) (p + 4) l
      else if op == op_mul then
        run2 (put c tar (op1 * op2)) (p + 4) l
      else c


run c = run2 c 0 (length c)
           
part1 = run progX

-- noun 0 to 99
-- verb 0 to 99 
searchFor noun verb nums =
  if noun > 99 then searchFor 0 (verb + 1) nums
  else if verb > 99 then []
  else 
    let n1 = put nums 1 noun in
      let n2 = put n1 2 verb in
        let n3 = run n2 in
             if (n3 !! 0) == 19690720 then
               (noun,verb, 100*noun + verb) : searchFor (noun +1) verb nums
             else searchFor (noun +1) verb nums
                   

expr =  [let progA = put progX 1 noun in
            let progB = put progA 2 verb in
              let out = (run progB) !! 0 in
                (out,noun,verb) | noun <- [0 .. 99] , verb <- [0 .. 99]]

-- destructuring tuple
win = filter (\ (o,n,v) -> o == 19690720 ) expr

-- map over solutions
part2 = map (\ (o,n,v) -> (100*n) + v) win


-- no io required in the solution  
main :: IO ()
main = do    
   return ()


-- -- f :: [String] -> [Int]
-- -- f = map read




