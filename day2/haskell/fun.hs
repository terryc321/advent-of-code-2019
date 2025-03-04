
import System.IO  
-- import Control.Monad

rev = reverse

-- tokenise contents of file into a sequence of number strings only
-- ignore newlines
-- ignore parenthesis
tok2 [] [] rs = rev rs
tok2 [] c rs = rev ((rev c) : rs)
tok2 (h:t) [] rs = if isDigit h then tok2 t ([h]) rs else tok2 t [] rs
tok2 (h:t) c rs = if isDigit h then tok2 t (h:c) rs else tok2 t [] ((rev c) : rs)
tok xs = tok2 xs [] []



-- split a string into multiple strings 
split2 [] [] rs = rev rs
split2 [] c rs = rev ((rev c) : rs)
split2 (h:t) [] rs = if h == '\n' then split2 t [] rs
                    else split2 t ([h]) rs
split2 (h:t) c rs = if h == '\n' then split2 t [] ((rev c) : rs)
                    else split2 t (h:c) rs
split xs = split2 xs [] []

--- all characters 0 to 9 inclusive
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

-- is list all digits ?
allDigits [] = True
allDigits (h:t) = if isDigit h then allDigits t else False

-- filter any strings not allDigits

-- convert string integer to an integer
s2num2 [] n = n
s2num2 ('0':t) n = s2num2 t (n*10 + 0)
s2num2 ('1':t) n = s2num2 t (n*10 + 1)
s2num2 ('2':t) n = s2num2 t (n*10 + 2)
s2num2 ('3':t) n = s2num2 t (n*10 + 3)
s2num2 ('4':t) n = s2num2 t (n*10 + 4)
s2num2 ('5':t) n = s2num2 t (n*10 + 5)
s2num2 ('6':t) n = s2num2 t (n*10 + 6)
s2num2 ('7':t) n = s2num2 t (n*10 + 7)
s2num2 ('8':t) n = s2num2 t (n*10 + 8)
s2num2 ('9':t) n = s2num2 t (n*10 + 9)
s2num s = s2num2 s 0

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




-- haskell !! operator gives nth element of list zeroth 0 is first element
-- position p
-- op !! p
-- op1 !! p + 1
-- op2 !! p + 2
-- tar !! p + 3

run2 code p len =
  if p < 0 then code 
  else if p >= len then code
  else let op = code !! p
           
       in
         if op == 99 then
           code
         else let op1 = code !! (code !! (p + 1))
                  op2 = code !! (code !! (p + 2))
                  tar = code !! (p + 3)
              in
                if op == 1 then
                  run2 (put code tar (op1 + op2)) (p + 4) len
                else if op == 2 then
                       run2 (put code tar (op1 * op2)) (p + 4) len
                     else code
                          


run code = run2 code 0 (length code)

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
                   
                      


-- -- list comprehension 
-- filter (\x -> (x !! 0) == 19690720)  [ (let n1 = put nums 1 noun in let n2 = put n1 2 verb in n3 = run n2)  | noun <- [0..99], verb <- [0..99] ]
  
  
main :: IO ()
main = do  
        let list = []
        handle <- openFile "../input" ReadMode
        contents <- hGetContents handle
        print contents
        --print (tok contents)
        let nums = map s2num (tok contents)
        print nums
        
        let n1 = (put nums 1 12) in
          let n2 = (put n1 2 2) in
            do print n2
               let n3 = run n2 in
                 print (n3 !! 0)

        print (searchFor 0 0 nums)
                 
        hClose handle
        


-- f :: [String] -> [Int]
-- f = map read


