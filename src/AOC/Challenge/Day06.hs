-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where


import           AOC.Common (firstRepeated, slidingWindows)
import           AOC.Solver ((:~>)(..))
import           Data.Foldable (toList)
import           Data.List (findIndex)
import           Data.Maybe (isNothing)

day06 :: Int -> String :~> Int
day06 n = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = fmap (+ n)
             . findIndex (isNothing . firstRepeated . toList)
             . slidingWindows n
    }

day06a :: String :~> Int
day06a = day06 4

day06b :: String :~> Int
day06b = day06 14
