
import System.IO  
-- import Control.Monad

rev = reverse

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

mass n =  (n `div` 3) - 2 

main = do  
        let list = []
        handle <- openFile "../input" ReadMode
        contents <- hGetContents handle
        print contents
        print (split contents)
        let nums = map s2num (filter allDigits (split contents))        
        print nums
        let masses = map mass nums
        print ("masses=",masses)
        let total = foldr (+) 0 masses 
        print ("total",total)
        hClose handle
        


-- f :: [String] -> [Int]
-- f = map read


