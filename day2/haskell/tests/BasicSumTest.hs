
module Main where
import BasicSum
import Test.HUnit
import qualified System.Exit as Exit

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

  
test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (basicSum 1 2))
 
tests :: Test
tests = TestList [TestLabel "test1" test1]


main2 :: IO ()
main2 = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

      
main :: IO ()
main = do
  _ <- main2  -- can we call main2 from main ? 
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess


 
