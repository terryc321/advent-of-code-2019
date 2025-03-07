module Main where


import Data.Array

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
    let arr = initialArray
    let arr' = updateArray arr 3 42  -- Update index 3 to value 42
    print arr'  -- Output: array (0,9) [(0,0),(1,0),(2,0),(3,42),(4,0),...]

  
    let initialMatrix = createMatrix 3 3 0  -- Create a 3x3 matrix filled with 0s
    let updatedMatrix = updateMatrix initialMatrix (1, 1) 5    -- Update the cell at (1, 1) to 5
    let updatedMatrix2 = updateMatrix updatedMatrix (2, 2) 22  -- Update the cell at (2, 2) to 22
    
    print initialMatrix
    print updatedMatrix
    print updatedMatrix2


