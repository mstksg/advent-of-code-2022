{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
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
import           Data.Bitraversable (bitraverse)
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP


data Instr = Instr { amount :: !Int, source :: !Int, dest :: !Int }
  deriving stock (Eq, Show, Generic)

instance NFData Instr

parseInput
    :: String
    -> Maybe (IntMap [Char], [Instr])
parseInput = bitraverse (parseGrid . lines) (parseInstr . lines)
         <=< listTup . splitOn "\n\n"
  where
    parseGrid  = Just . IM.fromList . zip [1..]
               . filter (not . null) . map (filter isLetter)
               . transpose
    parseInstr = traverse $
          fmap (\(V3 amount source dest) -> Instr{..})
        . listV3
      <=< traverse readMaybe
        . words
        . clearOut (not . isDigit)

runInstr
    :: (Int -> [a] -> [a] -> ([a], [a]))
    -> IntMap [a]
    -> Instr
    -> IntMap [a]
runInstr f stacks (Instr{..}) = IM.fromList [(source, xs'), (dest, ys')] `IM.union` stacks
  where
    xs = stacks IM.! source
    ys = stacks IM.! dest
    (xs', ys') = f amount xs ys

day05a :: _ :~> _
day05a = MkSol
    { sParse = parseInput
    , sShow  = id
    , sSolve = \(stacks, instrs) ->
          traverse listToMaybe
        . toList
        $ foldl' (runInstr shiftItems) stacks instrs
    }
  where
    shiftItems :: Int -> [a] -> [a] -> ([a],[a])
    shiftItems 0 xs    ys  = (xs, ys)
    shiftItems _ []    ys  = ([], ys)
    shiftItems n (x:xs) ys = shiftItems (n-1) xs (x:ys)

day05b :: _ :~> _
day05b = MkSol
    { sParse = sParse day05a
    , sShow  = id
    , sSolve = \(stacks, instrs) ->
          traverse listToMaybe
        . toList
        $ foldl' (runInstr shiftItems) stacks instrs
    }
  where
    shiftItems :: Int -> [a] -> [a] -> ([a],[a])
    shiftItems n xs ys  = (xs', zs ++ ys)
      where
        (zs,xs') = splitAt n xs
