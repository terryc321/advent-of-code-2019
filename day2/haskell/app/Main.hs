module Main where


import Data.Array
import qualified Data.Map as Map 
import qualified Data.Set as Set 


-- Create a fixed-size immutable array
initialArray :: Array Int Int
initialArray = listArray (0, 9) (replicate 10 0)  -- Array of size 10, initialized to 0

-- "Update" the array at a specific index
updateArray :: Array Int Int -> Int -> Int -> Array Int Int
updateArray arr index value = arr // [(index, value)]


-- Define a type for the 2D array
type Matrix a = Array (Int, Int) a

-- Create a 2D array with initial values
createMatrix :: Int -> Int -> a -> Matrix a
createMatrix rows cols initialValue = array ((0, 0), (rows-1, cols-1)) [((i, j), initialValue) | i <- [0..rows-1], j <- [0..cols-1]]

-- Update a specific cell in the matrix
updateMatrix :: Matrix a -> (Int, Int) -> a -> Matrix a
updateMatrix mat (i, j) newValue = mat // [((i, j), newValue)]


-- Example usage
main :: IO ()
main = do
    -- example of  Data.array
    let arr = initialArray
    let arr' = updateArray arr 3 42  -- Update index 3 to value 42
    print arr'  -- Output: array (0,9) [(0,0),(1,0),(2,0),(3,42),(4,0),...]

    -- example Data.Array
    let initialMatrix = createMatrix 3 3 0  -- Create a 3x3 matrix filled with 0s
    let updatedMatrix = updateMatrix initialMatrix (1, 1) 5    -- Update the cell at (1, 1) to 5
    let updatedMatrix2 = updateMatrix updatedMatrix (2, 2) 22  -- Update the cell at (2, 2) to 22
    
    print initialMatrix
    print updatedMatrix
    print updatedMatrix2

    --- Map effectively a hash map , all keys need to be the same , all values need to be same too ??
    let map1 = Map.fromList [("betty","555-2938"),("bonny","452-2928"),("lucile","205-2928")]
    print map1
    --- we can insert new value
    let map2 = Map.insert "terry" "723-3542" map1
    print map2
    --- can we over-write existing key value pair ?
    let map3 = Map.insert "betty" "999-8765" map2
    print map3
    --- can we see original map2 with
    print "can we see orignal betty from previous Map ?"
    print map2
    print "we can see the size of map with betty 999 "
    print $ Map.size map3

    
    --- explore some features of Data.Set
    let text1 = "I just had an anime dream. Anime .. Reality ... Are they so different??"
    let text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

    let set1 = Set.fromList text1
    let set2 = Set.fromList text2

    print "Here is the string of text1 :"
    print text1
    print "Here is the string of text2 :"
    print text2
    
    print "Here is set of characters from text1"
    print set1
    print "Here is set of characters from text2"
    print set2

    -- intersection
    print "Here is intersection of set 1 with set 2 "
    print $ Set.intersection set1 set2
    print "Here is intersection of set 2 with set 1 "
    print $ Set.intersection set2 set1
        
    -- difference
    print "Here is difference of set 1 with set 2 "
    print $ Set.difference set1 set2
    print "Here is difference of set 2 with set 1 "
    print $ Set.difference set2 set1

    -- union
    print "Here is union of set 1 with set 2 "
    print $ Set.union set1 set2

    print "Delete letter A from set 1 "
    let set3 = Set.delete 'A' set1
    print set3
    
    
    -- something to satisy checker return void
    return ()

    
    

  



