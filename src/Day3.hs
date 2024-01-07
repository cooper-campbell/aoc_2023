module Day3 (run_day) where

-- I will never return to this again, the second part of this question is pure evil

import Data.Char
import qualified Data.Set as S

import Common

-- Represents a number in the string
data BoxValue = 
  BoxValue {
            start :: Coordinate Int
           ,end :: Coordinate Int
           ,value :: Int
           }
           deriving(Show)

data Symbol = 
  Symbol {
          location :: Coordinate Int
         ,text :: Char
         }
         deriving(Show)

-- Returns  list of all numeric substrings in a string, non-numeric substrings are represented with '[]'
-- So you'll probably want to filter this by empty string on snd tuple
get_nums_subst :: Int -> String -> [(Int, String)]
get_nums_subst x [] = [(x, [])]
get_nums_subst index (f:s) = if isDigit f then (index, f : snd (head other_nums)) : (tail other_nums) else (index, []) : other_nums
  where new_index = index + 1
        other_nums = get_nums_subst new_index s

-- Returns a list of tuples with (Index, Digits, Value) while filtering empty values
get_num_list :: [(Coordinate Int, String)] -> [BoxValue]
get_num_list xs = [BoxValue coord (Coordinate (p1 coord) (p2 coord + length y - 1)) (read y) | (coord,y) <- xs, y /= ""]

symbols_list :: Coordinate Int -> String -> [Symbol]
symbols_list _ [] = []
symbols_list coord (c:s) = if isSymbol then (Symbol coord c) : remaining else remaining
  where isSymbol = (not $ isDigit c) && c /= '.'
        newcoord = Coordinate (p1 coord) (1 + p2 coord)
        remaining = symbols_list newcoord s

return_value :: BoxValue -> Coordinate Int -> Bool
return_value box c = (p1 $ start box) == (p1 c) && (elem (p2 c) [(p2 $ start box) .. (p2 $ end box)])

product_list :: Num a => [a] -> a
product_list [] = 1
product_list (x:xs) = x * product_list xs

run_day :: String -> [String]
run_day input = do
    [("Part 1 : ") ++ (show $ sum part1), ("Part 2 : ") ++ (show $ sum gears)]
    where per_line = lines input
          upper_box = Coordinate ((length $ head per_line) - 1) ((length per_line) - 1)
          lower_box = Coordinate 0 0
          bounding_box = \(BoxValue start end value) -> points_surrounding_box_limit (lower_box, upper_box) (start, end)

          -- This is most of the magic, this is a list of coordinates and their numeric value from the list of strings
          num_maps = get_num_list $ concat $ [map (\(i,s) -> ((Coordinate row i), s)) (get_nums_subst 0 l) | (row,l) <- zip [0..] per_line]
          symbols = concat [symbols_list (Coordinate row 0) l | (row,l) <- zip [0..] per_line]

          symbols_coords = map (\(Symbol c t) -> c) symbols

          part1 = map (\(value, _) -> value)
            $ filter (\(_, search_list) -> (length [x | x <- search_list, elem x symbols_coords]) /= 0)
            $ map (\(bv) -> (value bv, bounding_box bv)) num_maps

          -- part 2, i hate this
          num_maps_coords = map(\(BoxValue start end value) -> [Coordinate x y | x <- [p1 start .. p1 end], y <- [p2 start .. p2 end]]) num_maps
          gear_candidate = map (\(Symbol c t) -> c) $ filter (\(Symbol c t) -> t == '*') symbols
          gears = map (\y -> product_list $ map (\x -> snd x) y) 
                $ filter (\l -> length l == 2)
                $ map (\(c, searches) -> S.toList $ S.fromList $ [(id, value box) | (id,box) <- zip [0..] num_maps, s <- searches, return_value box s])
                $ map (\c -> (c, points_surrounding_coords_limit (lower_box, upper_box) c)) gear_candidate
