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

import           AOC.Common (countTrue)
import           AOC.Common.Point (Point)
import           AOC.Solver ((:~>)(..))
import           Data.Char (digitToInt)
import           Data.IntMap (IntMap)
import           Data.List (findIndex, transpose)
import           Linear (V2(..), V4(..))
import           Safe.Foldable (maximumMay)
import qualified Data.IntMap as IM

crossSections
    :: String
    -> (IntMap [Int], IntMap [Int])
crossSections xs = ( IM.fromList (zip [0..] horizLines)
                   , IM.fromList (zip [0..] vertLines)
                   )
  where
    horizLines = map (map digitToInt) $ lines xs
    vertLines  = transpose horizLines

visibility
    :: (IntMap [Int], IntMap [Int])
    -> Point
    -> V4 (Bool, Int)
visibility (horizLines, vertLines) (V2 x y) =
    reshape <$> V4 toUp toRight toDown toLeft
  where
    (reverse->toLeft, hereAndToRight) = splitAt x (horizLines IM.! y)
    (reverse->toUp  , tail->toDown  ) = splitAt y (vertLines  IM.! x)
    here    = head hereAndToRight
    toRight = tail hereAndToRight
    reshape ls = case findIndex (>= here) ls of
      Nothing -> (True , length ls)
      Just i  -> (False, i + 1    )

day08a :: _ :~> _
day08a = MkSol
    { sParse = Just . crossSections
    , sShow  = show
    , sSolve = \cs@(horiz, vert) -> Just . countTrue or $ [
          fst <$> visibility cs (V2 x y)
        | y <- IM.keys horiz
        , x <- IM.keys vert
        ]
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just . crossSections
    , sShow  = show
    , sSolve = \cs@(horiz, vert) -> maximumMay [
          product $ snd <$> visibility cs (V2 x y)
        | y <- IM.keys horiz
        , x <- IM.keys vert
        ]
    }
