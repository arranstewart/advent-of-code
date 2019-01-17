

import qualified Data.List as L
import qualified Data.Set as S
import           Data.Set (Set)

import NumberTheory
import Lens.Micro.Platform hiding (view)
import Lens.Micro.Extras
import Control.Arrow



getInput = 
  map parseDir <$> readFile "input.txt"

type G = GaussInt Int

parseDir :: Char -> G
parseDir c = case c of
  '>' -> 1    :+ 0
  '<' -> (-1) :+ 0
  '^' -> 0    :+ 1
  'v' -> 0    :+ (-1)
  _   -> error "bad char"

visit :: Foldable t => t G -> (G, Set G)
visit dirs = 
  L.foldl' (flip f) (startPos, S.singleton startPos) dirs
  where
    startPos = 0 :+ 0

    f :: G -> (G, Set G) -> (G, Set G)
    f dir (pos, acc) = let pos' = pos .+ dir
                       in  (pos', pos' `S.insert` acc)

partA :: Foldable t => t G -> Int
partA dirs =
  let (endPos, visited) = visit dirs
  in  S.size visited

-- get even and odd positions
partitionPositions :: [b] -> ([b], [b])
partitionPositions ds =
  map snd *** map snd $
      L.partition evenPos $ zip [0..] ds
  where
    evenPos = (== 0) . (`mod` 2) . view _1

partB :: [G] -> Int
partB dirs =  
  let (dirs1, dirs2)      = partitionPositions dirs
      (endPos1, visited1) = visit dirs1
      (endPos2, visited2) = visit dirs2

  in S.size $ visited1 `S.union` visited2
        

main = do
  dirs <- getInput
  putStrLn $ "part A: " ++ show (partA dirs)
  putStrLn $ "part B: " ++ show (partB dirs)



