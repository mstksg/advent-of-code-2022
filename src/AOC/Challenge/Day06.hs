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


import           AOC.Solver ((:~>)(..))
import           Data.List (transpose)
import qualified Data.Set as S

day06 :: Int -> String :~> Int
day06 n = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . solve
    }
  where
    solve xs = length (takeWhile ((< n) . S.size . S.fromList) chunks) + n
      where
        chunks = transpose $ map (`drop` xs) [0..n-1]

day06a :: String :~> Int
day06a = day06 4

day06b :: String :~> Int
day06b = day06 14
