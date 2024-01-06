module Day2 (run_day) where

-- Necessary for isSuffixOf
import Data.List

data Draw = Draw { red :: Int, green :: Int, blue :: Int } deriving(Show)
data Game = Game { id :: Int, rolls :: [Draw] } deriving(Show)

-- Converts input string to the Draw struct
string_to_roll :: String -> Draw
string_to_roll "" = Draw 0 0 0
string_to_roll s = Draw red green blue
  where ind_colors = map drop_whitespace $ split ',' s
        red   = sum [read (head (split ' ' x)) :: Int | x <- filter (isSuffixOf "red") ind_colors]
        blue  = sum [read (head (split ' ' x)) :: Int | x <- filter (isSuffixOf "blue") ind_colors]
        green = sum [read (head (split ' ' x)) :: Int | x <- filter (isSuffixOf "green") ind_colors]

-- Splits string into list of string based on char
split :: Char -> String -> [String]
split _ [] = []
split c s = front : (split c remainder)
  where front = takeWhile (/=c) s
        remainder = drop (1 + length front) s

-- Removes leading whitespace from string
drop_whitespace :: String -> String
drop_whitespace s = dropWhile (\c -> c == ' ') s

-- Converts string to Game struct, called for each line
convert_to_game_structure :: String -> Game
convert_to_game_structure game_str = Game (read game_num :: Int) information
  where (header:rest) = (split ':' game_str)
        game_num = last (split ' ' header)
        information = map string_to_roll $ map drop_whitespace $ concat $ map (split ';') rest

-- Returns true of every boolean in the list is true
all_true :: [Bool] -> Bool
all_true [] = True
all_true (x:xs) = if x then all_true xs else False

--Applies constraing as given in part 1, unused for part 2
draw_fits_constraint :: Draw -> Draw -> Bool
draw_fits_constraint constraint d = red_valid && green_valid && blue_valid
  where red_valid = red d <= red constraint
        green_valid = green d <= green constraint
        blue_valid = blue d <= blue constraint

run_day :: String -> [String]
run_day input = do
  let games = lines input 

  -- Get game into structured format
  let s = map convert_to_game_structure games
  
  -- For each roll in game, ensure it meets constraint, filter those that dont, make list of only the Id's of remaining, sum it
  let part_1 = sum $ map (\game -> Day2.id game) $ filter (\game -> all_true [constrain d | d <- (rolls game)]) s
  -- For each set of draws in game, calculate the maximum of each color, calculate their product, sum the products
  let part_2 = sum $ map (\game -> (max_color red (rolls game)) * (max_color green (rolls game)) * (max_color blue (rolls game))) s

  ["Part 1: " ++ show part_1, "Part 2: " ++ show part_2]

    where part_1_constraint = Draw 12 13 14
          constrain = draw_fits_constraint part_1_constraint
          max_color = \c -> \x -> maximum [(c d) | d <- x]
