{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day25 (
    day25a
  , decToSnafu
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

snafuToDec :: [Char] -> Int
snafuToDec = sum . map go . zip [0..] . reverse
  where
    go (i, c) = case c of
        '1' -> j
        '2' -> 2*j
        '-' -> -j
        '=' -> -2*j
        '0' -> 0
        _    -> undefined
      where
        j = 5^(i::Int)

decToSnafu :: Int -> [Char]
decToSnafu = reverse . unfoldr go
  where
    go i = case i `divMod` 5 of
      (0,j) | j <= 0 -> Nothing
      (j,0) -> Just ('0', j)
      (j,1) -> Just ('1', j)
      (j,2) -> Just ('2', j)
      (j,3) -> Just ('=', j+1)
      (j,4) -> Just ('-', j+1)
      _     -> Nothing

day25a :: _ :~> _
day25a = MkSol
    { sParse = Just . lines
    , sShow  = id
    , sSolve = Just . decToSnafu . sum . map snafuToDec
    }

-- 1=-=2--12==000=00-
