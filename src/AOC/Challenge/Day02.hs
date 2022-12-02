-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Common (listTup)
import           AOC.Solver ((:~>)(..))
import           Data.Finite (Finite, getFinite)
import           Data.Maybe (mapMaybe)

type Z3 = Finite 3

matchLetter :: Char -> Maybe Z3
matchLetter = \case
    'A' -> Just 0
    'B' -> Just 1
    'C' -> Just 2
    'X' -> Just 0
    'Y' -> Just 1
    'Z' -> Just 2
    _   -> Nothing

parseLine :: String -> Maybe (Z3, Z3)
parseLine = listTup . mapMaybe matchLetter

scoreMatch :: Z3 -> Z3 -> Integer
scoreMatch x y = baseScore + matchScore
  where
    baseScore  = getFinite y + 1
    matchScore = getFinite (y - x + 1) * 3

day02a :: [(Z3, Z3)] :~> Integer
day02a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . sum . map (uncurry scoreMatch)
    }

-- y becomes x + y + 2 in scoreMatch
scoreMatch2 :: Z3 -> Z3 -> Integer
scoreMatch2 x y = baseScore + matchScore
  where
    baseScore  = getFinite (x + y + 2) + 1
    matchScore = getFinite y * 3

day02b :: [(Z3, Z3)] :~> Integer
day02b = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . sum . map (uncurry scoreMatch2)
    }
