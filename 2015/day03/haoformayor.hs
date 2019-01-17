
-- /u/haoformayor 
-- https://www.reddit.com/r/adventofcode/comments/3v8roh/day_3_solutions/cxloynd/ 


{-# LANGUAGE NoImplicitPrelude #-}
import BasePrelude

folder (x, y) '<' = (x - 1, y)
folder (x, y) '>' = (x + 1, y)
folder (x, y) '^' = (x, y + 1)
folder (x, y) 'v' = (x, y - 1)

odds xs = [x | (x, i) <- zip xs (cycle [True, False]), i]
evens xs = [x | (x, i) <- zip xs (cycle [False, True]), i]

points = scanl folder (0,0)
count = length . nub . sort
part1 = count . points
part2 = count . ((++) . points . odds <*> points . evens)


main = do
  input <- readFile "input.txt"
  putStrLn $ "Part A: " ++ show (part1 input)
  putStrLn $ "Part B: " ++ show (part2 input)

    
