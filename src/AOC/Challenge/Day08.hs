{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

isVisible
    :: Map Point Int
    -> Point
    -> Bool
isVisible mp (V2 x y)  = all (< ht) toLeft
                      || all (< ht) toRight
                      || all (< ht) toUp
                      || all (< ht) toDown
  where
    ht = mp M.! V2 x y
    horizLine = M.fromList . mapMaybe (\((V2 x' y'),k) -> (x',k) <$ guard (y' == y)) . M.toList $ mp
    (toLeft, tail->toRight) = splitAt x $ toList $ horizLine
    vertLine = M.fromList . mapMaybe (\((V2 x' y'),k) -> (y',k) <$ guard (x' == x)) . M.toList $ mp
    (toUp, tail->toDown) = splitAt y $ toList $ vertLine

scenic
    :: Map Point Int
    -> Point
    -> Int
scenic mp (V2 x y)  = maybe (length toLeft) (+1) (findIndex (>= ht) (reverse toLeft))
                    * maybe (length toRight) (+1) (findIndex (>= ht) toRight)
                    * maybe (length toUp) (+1) (findIndex (>= ht) (reverse toUp))
                    * maybe (length toDown) (+1) (findIndex (>= ht) toDown)
  where
    ht = mp M.! V2 x y
    horizLine = M.fromList . mapMaybe (\((V2 x' y'),k) -> (x',k) <$ guard (y' == y)) . M.toList $ mp
    (toLeft, tail->toRight) = splitAt x $ toList $ horizLine
    vertLine = M.fromList . mapMaybe (\((V2 x' y'),k) -> (y',k) <$ guard (x' == x)) . M.toList $ mp
    (toUp, tail->toDown) = splitAt y $ toList $ vertLine



day08a :: _ :~> _
day08a = MkSol
    { sParse = Just . parseAsciiMap digitToIntSafe
    , sShow  = show
    , sSolve = \mp -> Just $ M.size $ M.filterWithKey (\k _ -> isVisible mp k)  mp
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = sParse day08a
    , sShow  = show
    , sSolve = \mp -> maximumMay .   map (scenic mp) $ (M.keys mp)
    }
