{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day09 (
    day09a
  , day09b
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

day09a :: [(Dir, Int)] :~> _
day09a = MkSol
    { sParse = Just . mapMaybe (fmap (bimap (parseDir' . head) read) . listTup . words) . lines
    , sShow  = show
    -- , sShow  = ('\n':) . displayAsciiMap '.' . M.fromList . flip zip (cycle ['a'..'z'])
    -- , sShow  = displayAsciiSet ' ' '#'
    -- , sSolve = Just . snd . mapAccumL go 0 . scanl' (+) 0 . concatMap (\(d, i) -> replicate i (dirPoint d))
    , sSolve = Just . S.size . S.fromList . lagMe . scanl' (+) 0 . concatMap (\(d, i) -> replicate i (dirPoint d))
    }
  where
    lagMe = snd . mapAccumL go 0
    go t h = (t+delta, t+delta)
      where
        dist = maximum $ fmap abs (h - t)
        delta = if dist > 1 then signum $ h - t else 0
          -- V2 2 0 -> V2 1 0
          -- V2 (-2) 0 -> V2 (-1) 0
          -- V2 0 2 -> V2 0 1
          -- V2 0 (-2) -> V2 0 (-1)

          -- V2 1 2 -> V2 1 1
          -- V2 1 (-2) -> V2 1 (-1)
          -- V2 2 1 -> V2 1 1
          -- V2 (-2) 1 -> V2 (-1) 1
          -- V2 (-1) 2 -> V2 (-1) 1
          -- V2 (-1) (-2) -> V2 (-1) (-1)
          -- V2 2 (-1) -> V2 1 (-1)
          -- V2 (-2) (-1) -> V2 (-1) (-1)
          -- _ -> 0
        -- t' = minimumBy (comparing (sum . fmap abs . (h -))) $ cardinalNeighbs t
        -- dist = maximum $ fmap abs (h - t)
        -- t' = if dist < 2
        --        then t
        --        else minimumBy (comparing (maximum . fmap abs . (h -))) $ cardinalNeighbs t



-- parseDir :: Char -> Maybe Dir
-- L 2
-- R 2
-- U 1
-- R 2
-- U 2
-- D 2
-- U 1
-- L 1
-- U 1
-- L 1
-- D 1

day09b :: _ :~> _
day09b = MkSol
    { sParse = Just . mapMaybe (fmap (bimap (parseDir' . head) read) . listTup . words) . lines
    , sShow  = show
    -- , sShow  = ('\n':) . displayAsciiMap '.' . M.fromList . flip zip (cycle ['a'..'z'])
    -- , sShow  = displayAsciiSet ' ' '#'
    -- , sSolve = Just . snd . mapAccumL go 0 . scanl' (+) 0 . concatMap (\(d, i) -> replicate i (dirPoint d))
    -- , sSolve = Just . (!! 2) . iterate lagMe . scanl' (+) 0 . concatMap (\(d, i) -> replicate i (dirPoint d))
    , sSolve = Just . S.size . S.fromList . (!! 9) . iterate lagMe . scanl' (+) 0 . concatMap (\(d, i) -> replicate i (dirPoint d))
    }
  where
    lagMe = snd . mapAccumL go 0
    go t h = (t+delta, t+delta)
      where
        dist = maximum $ fmap abs (h - t)
        delta = if dist > 1 then signum $ h - t else 0











