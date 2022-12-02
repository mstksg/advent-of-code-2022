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

scoreMatches
    :: (Z3 -> Z3 -> Z3)  -- ^ Get shape score
    -> (Z3 -> Z3 -> Z3)  -- ^ Get outcome score
    -> [(Z3, Z3)]
    -> Integer
scoreMatches f g = sum . map go
  where
   go (x, y) = getFinite (f x y) + getFinite (g x y) * 3 + 1

parseLine :: String -> Maybe (Z3, Z3)
parseLine = listTup . mapMaybe matchLetter

day02a :: [(Z3, Z3)] :~> Integer
day02a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . scoreMatches (\_ y -> y) (\x y -> y - x + 1)
    }

day02b :: [(Z3, Z3)] :~> Integer
day02b = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . scoreMatches (\x y -> x + y + 2) (\_ y -> y)
    }
