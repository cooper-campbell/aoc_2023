module Day4 (runDay) where

import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Text.Read


type Game = (Maybe Int, [Maybe Int], [Maybe Int])

gameWinning, gameHas :: Game -> [Maybe Int]
gameWinning (_,w,_) = w
gameHas (_,_,h) = h

separator :: Char
separator = '|'

filterNonDigits :: String-> [(Bool, String)]
filterNonDigits [] = [(False, [])]
filterNonDigits (c:s) = if isDigit c 
                             then (False, (c : (snd (head remainder)))) : tail remainder
                             else (c == separator, []) : remainder
  where remainder = filterNonDigits s

listTogame :: [Maybe Int] -> Game
listTogame (g_id:rest) = (g_id,winning,has)
  where winning = takeWhile isJust rest
        has = drop (1 + length winning) rest

gameToMatches :: Game -> [Maybe Int]
gameToMatches g = [x | x <- gameHas g, x `elem` gameWinning g]

calcNumCards :: Map.Map Int Int -> [(Int,Int)] -> [Int]
calcNumCards _ [] = []
calcNumCards m (h:t) = copies : calcNumCards new_map t
  where (index, matches) = h
        copies = 1 + fromMaybe 0 (Map.lookup index m)
        new_map = Map.delete index $ foldr alteration m keys_to_alter
        keys_to_alter = map (index +) [1..matches]
        alteration = Map.alter (\m -> Just (copies + fromMaybe 0 m))

runDay :: String -> [String]
runDay input = do
    [show answer1, show answer2]
    where per_line = lines input

          part1 = map (sum . (fmap (\x -> 2^x)) . (\x -> if x >= 0 then Just x else Nothing) . (subtract 1) . length) cards
          -- In the interest of not changing part1 after submission, we essentially re-add the game id, by index.
          -- I could change the pipeline, but I want to preserve the original part1 solution
          part2 = map (\(x,y) -> (x,length y)) $ zip [0..] cards

          answer1 = sum part1
          answer2 = sum $ calcNumCards Map.empty part2

          -- Starting point for inputs
          cards = map pipeline per_line
          pipeline = 
            gameToMatches
            . listTogame
            . map (\(_,x) -> readMaybe x :: Maybe Int)
            . filter (\(s,x) -> s || (x /= []))
            . filterNonDigits
