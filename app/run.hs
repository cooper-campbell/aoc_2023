{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
import System.Environment
import Text.Printf
import Data.Maybe

-- Import the days
import Day1 (run_day)
import Day2 (run_day)
import Day3 (run_day)

completed_days = [
  1,
  2,
  3
                 ]



run_days :: [Int] -> IO ()
run_days [] = putStrLn "Finished running days"
run_days (d:rest) = do {
                       content <- readFile (printf "inputs/day%d.txt" d)
                       ; putStrLn ("Day: " ++ show d ++ " " ++ show ((solutions !! (d-1)) content))
                       ; run_days rest
                       }
  where
    solutions = [
                Day1.run_day,
                Day2.run_day,
                Day3.run_day
                ]

main = do
  args <- getArgs
  let days = catMaybes [if (read x :: Int) `elem` completed_days then Just (read x :: Int) else Nothing | x <- args]
  putStrLn ("Ready for days " ++ (show days))
  
  run_days days
