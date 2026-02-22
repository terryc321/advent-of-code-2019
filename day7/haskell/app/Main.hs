module Main where

import Data.Sequence as S

{-
Your wish is my IO ().
If I break, you can:
  1. Restart:           M-x haskell-process-restart
  2. Configure logging: C-h v haskell-process-log (useful for debugging)
  3. General config:    M-x customize-mode
  4. Hide these tips:   C-h v haskell-process-show-debug-tips
-}

--- C-c C-l to load into ghci
-- input :: S.Seq Integer
--- changed element 0 to 123 was 1
--- changed last element to 321 was 0
input :: Seq Integer 
input = S.fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0]
--DS.length (DS.fromList input) => 133
--S.length (S.fromList input) => 133
--S.lookup 0 -- Just 1 -- back to monads now 
--S.lookup 132 -- Just 0 -- back to monads now

--- index

--- 1 adds
--- 2 multiplies
--- 99 halts

--- tape === Seq Integer

-- data Tape = Tape_Seq (Seq Integer)
-- data Index = Index_Int Int 
data State = Sok (Seq Integer) Integer
             | Shalt (Seq Integer) Integer
             | Sbad (Seq Integer) Integer


             

--- some kind of state ?
--- t :: tape
--- i index into tape
--- ?? history of moves
sim :: State -> State 
sim (Sbad s i ) = Sbad s i
sim (Shalt s i ) = Shalt s i
sim (Sok s i ) = let len = S.length s
                 in if (i >= 0 && i < len) then
                      case S.lookup i s of 
                      Nothing -> Sbad s i
                      Just n -> case n of
                        1 -> sim_add s i 
                        2 -> sim_mul s i
                        99 -> Shalt s i
                        _ -> Sbad s i
                    else Sbad s i


sim_add s i = let len = S.length s
              in case (S.lookup (i+1) s, S.lookup (i+2) s, S.lookup (i+3) s) of
                (Just a,Just b, Just c) -> case c of
                                             if (c >= 0 && c < len) then
                                               let s2 = S.update c (a+b) s
                                               in sim (Sok s2 (i + 4))
                                             else Sbad s i
                _ -> Sbad s i 
                                                 

sim_mul s i = let len = S.length s
              in case (S.lookup (i+1) s, S.lookup (i+2) s, S.lookup (i+3) s) of
                (Just a,Just b, Just c) -> case c of
                                             if (c >= 0 && c < len) then
                                               let s2 = S.update c (a*b) s
                                               in sim (Sok s2 (i + 4))
                                             else Sbad s i
                _ -> Sbad s i 
                                                 




main :: IO ()
main = putStrLn "Hello, Haskell!"
