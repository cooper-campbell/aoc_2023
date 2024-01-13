{-# LANGUAGE ExtendedDefaultRules #-}
import System.Environment
import Text.Printf
import Data.Maybe

-- Import the days
import Day1 (run_day)
import Day2 (run_day)
import Day3 (run_day)
import Day4 (runDay)

completedDays :: [Int]
completedDays = [
  1,
  2,
  3,
  4
                 ]



runDays :: [Int] -> IO ()
runDays [] = putStrLn "Finished running days"
runDays (d:rest) = do {
                       content <- readFile (printf "inputs/day%d.txt" d)
                       ; putStrLn ("Day: " ++ show d ++ " " ++ show ((solutions !! (d-1)) content))
                       ; runDays rest
                       }
  where
    solutions = [
                Day1.run_day,
                Day2.run_day,
                Day3.run_day,
                Day4.runDay
                ]

main :: IO ()
main = do
  args <- getArgs
  let days = catMaybes [if (read x :: Int) `elem` completedDays then Just (read x :: Int) else Nothing | x <- args]
  putStrLn ("Ready for days " ++ show days)
  
  runDays days
