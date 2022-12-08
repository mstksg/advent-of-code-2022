-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Common (countTrue, digitToIntSafe)
import           AOC.Solver ((:~>)(..))
import           Data.List (transpose, mapAccumL, foldl')
import           Safe.Foldable (maximumMay)
import qualified Data.Map as M

onSightLines
    :: ([Int] -> [a])
    -> (a -> a -> a)
    -> [[Int]]
    -> [a]
onSightLines f g rows = concat $
    foldl'
      ((zipWith . zipWith) g)
      leftLines
      [rightLines, upLines, downLines]
  where
    leftLines  = map f rows
    rightLines = map (reverse . f . reverse) rows
    upLines    = transpose $ map f (transpose rows)
    downLines  = transpose $ map (reverse . f . reverse) (transpose rows)

day08a :: [[Int]] :~> Int
day08a = MkSol
    { sParse = (traverse . traverse) digitToIntSafe . lines
    , sShow  = show
    , sSolve = Just . countTrue id
             . onSightLines (snd . mapAccumL propagateSight (-1)) (||)
    }
  where
    propagateSight i x = (max i x, x > i)

day08b :: [[Int]] :~> Int
day08b = MkSol
    { sParse = (traverse . traverse) digitToIntSafe . lines
    , sShow  = show
    , sSolve = maximumMay
             . onSightLines (snd . mapAccumL findSight M.empty . zip [0..]) (*)
    }
  where
    findSight lastSeeable (i, x) =
      ( M.fromList ((,i) <$> [0..x]) `M.union` lastSeeable
      , i - M.findWithDefault 0 x lastSeeable
      )
