{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
    day02a
  , day02b
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

data Throw = Rock | Paper | Scissors
  deriving (Show, Eq, Generic)

instance NFData Throw

matchLetter 'A' = Rock
matchLetter 'B' = Paper
matchLetter 'C' = Scissors
matchLetter 'X' = Rock
matchLetter 'Y' = Paper
matchLetter 'Z' = Scissors

data Result = Lose | Draw | Win
  deriving (Show, Eq, Generic)

instance NFData Result

matchResult 'X' = Lose
matchResult 'Y' = Draw
matchResult 'Z' = Win


scoreMatch x y = sbase y + smatch x y
  where
    sbase = \case
      Rock -> 1
      Paper -> 2
      Scissors -> 3

    smatch Rock Rock = 3
    smatch Rock Paper = 6
    smatch Rock Scissors = 0
    smatch Paper Rock = 0
    smatch Paper Paper = 3
    smatch Paper Scissors = 6
    smatch Scissors Rock = 6
    smatch Scissors Paper = 0
    smatch Scissors Scissors = 3

day02a :: _ :~> _
day02a = MkSol
    { sParse = Just . map (\[a,_,b] -> (matchLetter a,matchLetter b)) . lines
    , sShow  = show
    , sSolve = Just . sum . map (uncurry scoreMatch)
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = Just . map (\[a,_,b] -> (matchLetter a,matchResult b)) . lines
    , sShow  = show
    , sSolve = Just . sum . map (uncurry go)
    }
  where
    go Rock Lose = scoreMatch Rock Scissors
    go Rock Draw = scoreMatch Rock Rock
    go Rock Win = scoreMatch Rock Paper
    go Paper Lose = scoreMatch Paper Rock
    go Paper Draw = scoreMatch Paper Paper
    go Paper Win = scoreMatch Paper Scissors
    go Scissors Lose = scoreMatch Scissors Paper
    go Scissors Draw = scoreMatch Scissors Scissors
    go Scissors Win = scoreMatch Scissors Rock
