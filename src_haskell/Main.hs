module Main where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T.IO
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Data.Maybe
import Debug.Trace
import qualified Data.List as L
import Control.Monad.ST
import Control.Monad
import System.Environment -- needed for getArgs

type Board = V.Vector [Char]

main :: IO ()
main = do
   [fname] <- getArgs
   f <- T.IO.readFile fname
   let txt = T.lines f
   T.IO.putStrLn $ head txt
   let solutions = map solveSudoku txt
   let first_result = head solutions
   mapM_ T.IO.putStrLn solutions
 

-- Return a solved sudoku, if there is a solution
solveSudoku :: T.Text -> T.Text
solveSudoku problem = displayBoard $ solve blankBoard problem

solve :: Board -> T.Text -> Maybe Board
solve board problem = let pairs = filter noDot (zip [0..80] (T.unpack problem)) in do
                        board <- assign board pairs
                        board <- search board
                        return board
                      where noDot = \(idx, val) -> val /= '.'

assign :: Board -> [(Int, Char)] -> Maybe Board
assign board [] = Just board
assign board pairs = foldM assignVal board pairs

assignVal :: Board -> (Int, Char) -> Maybe Board
assignVal board (idx, val) = propagateConstraints (board V.// [(idx, [val])]) idx val

propagateConstraints :: Board -> Int -> Char -> Maybe Board
propagateConstraints board idx val = foldM (removeFromBoard val) board (neighboursOf idx)

removeFromBoard :: Char -> Board -> Int -> Maybe Board
removeFromBoard val board idx = let old_entry = board V.! idx in
  if (not $ elem val old_entry) then Just board
  else do
    let new_entry = L.delete val (board V.! idx)
    case new_entry of [] -> Nothing
                      [x] -> propagateConstraints (board V.// [(idx, new_entry)]) idx x
                      _ -> return $ board V.//[(idx, new_entry)]

neighboursOf :: Int -> [Int]
neighboursOf idx = L.delete idx $ consecutives idx `L.union` offBy9 idx `L.union` boxes idx

search :: Board -> Maybe Board
search board = let result = bestSearchOption board in
  -- All entries have 1 value.  We win!
  case result of Nothing -> Just board
  -- Otherwise, try to apply all possible values in val at position idx
                 -- listToMaybe returns Just the first element of the list, or
                 -- Nothing if the list is empty
                 Just (idx, vals) -> listToMaybe solutions
                    -- catMaybes drops all of the Nothings and makes a list of
                    -- values
                    where solutions = catMaybes $ map (\val -> tryVal board (idx, val)) vals

-- Assign the given value at the given location, and then try to solve the rest
-- of the board.
tryVal :: Board -> (Int, Char) -> Maybe Board
tryVal board val = do
  firstboard <- assignVal board val
  secondboard <- search firstboard
  return secondboard

-- Find the location in the vector with the fewest (but >1) options left
-- If all entries have 0 or 1 values left in them, returns Nothing
bestSearchOption :: Board -> Maybe (Int, [Char])
bestSearchOption board = V.ifoldr maxfn Nothing board where
  maxfn idx val prev_result = if (length val) < 2 then prev_result else
    case prev_result of Nothing -> Just (idx, val)
                        Just (prev_idx, prev_val) -> if (length val) < (length prev_val) then Just (idx,val) else prev_result

-- Locations in the same row as idx
consecutives :: Int -> [Int]
consecutives idx = map (\i -> i + base_idx) [0..8]
                    where base_idx = idx - (idx `mod` 9)

-- Locations in the same column as idx
offBy9 :: Int -> [Int]
offBy9 idx = map (\i -> i*9 + base_idx) [0..8]
              where base_idx = idx `mod` 9

-- Locations in the same box as idx
boxes :: Int -> [Int]
boxes idx = [starti + i + 9*(startj + j) | i <- [0..2], j <- [0..2]]
              where x = idx `mod` 9
                    y = idx `div` 9
                    starti = x - x `mod` 3 
                    startj = y - y `mod` 3

displayBoard :: Maybe Board -> T.Text
displayBoard (Just v) = T.pack $ L.intercalate "\n" $ map (L.intercalate " | ") list_of_lists
                       where list_of_lists = vecToLoL v
displayBoard Nothing = T.pack "No solution"

-- Convert vector to list of lists
vecToLoL :: V.Vector [Char] -> [[[Char]]]
vecToLoL v = [[v V.! (i+9*j) | i <- [0..8]] | j <- [0..8]]

digits :: String
digits = "123456789"

blankBoard :: Board
blankBoard = V.replicate 81 digits

