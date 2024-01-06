{-# LANGUAGE OverloadedStrings #-}
module Day1 (run_day) where
import qualified Data.Text.Internal.Search as TS
import qualified Data.Text as T
-- Needed for sortBy
import Data.List

-- Part one only asked for char literals 0-9 to be considered
map_part_1, map_part_2 :: [(T.Text, Int)]
map_part_1 = [ ("0", 0), ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9) ]
-- Part two asks for consideration of spelling numbers 1-9
map_part_2 = concat [map_part_1, extra_part_2]
  where extra_part_2 = [ ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9) ]

-- Returns the first and last text-to-number replacements in a string from a map
get_first_last_num :: [(T.Text, Int)] -> T.Text -> (Int, Int)
get_first_last_num m s = if (length sorted) == 0 then (0,0) else (snd (head sorted), snd (last sorted))
  where matches = [(index,val) | (search, val) <- m, index <- TS.indices search s]
        sorted = sortBy (\x y -> compare (fst x) (fst y)) matches

run_day :: String -> [String]
run_day input = do
  let texts = [T.pack x | x <- lines input]
  -- Calculate part 1 and 2
  let part_1 = sum $ map combine $ map (get_first_last_num map_part_1) texts
  let part_2 = sum $ map combine $ map (get_first_last_num map_part_2) texts
  [("Part 1 : " ++ (show part_1)), ("Part 2 : " ++ (show part_2))]
    where
      combine = \(tens, ones) -> 10 * tens + ones
