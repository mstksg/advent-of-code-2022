{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day20 (
    day20a
  , day20b
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

day20a :: _ :~> _
day20a = MkSol
    { sParse = traverse (readMaybe @Int) . lines
    , sShow  = show
    , sSolve = grove . go 0 . Seq.fromList . zip [0..]
    }
  where
    grove xs = do
        i <- Seq.elemIndexL 0 xs
        let ixs = (`mod` n) . (+ i) <$> [1000,2000,3000]
        vs <- traverse (`Seq.lookup` xs) ixs
        pure $ sum vs
      where
        n = Seq.length xs
    go :: Int -> Seq.Seq (Int, Int) -> Seq.Seq Int
    go i xs = case postXsMaybe of
        (_, v) Seq.:<| postXs ->
          let newIx = (Seq.length preXs + v) `mod` (n-1)
          in  go (i+1) $ Seq.insertAt newIx (i,v) (preXs <> postXs)
        _ -> snd <$> xs
      where
        n = Seq.length xs
        (preXs, postXsMaybe) = Seq.spanl ((/= i) . fst) xs

magic :: Int
magic = 811589153

day20b :: _ :~> _
day20b = MkSol
    { sParse = sParse day20a
    , sShow  = show
    , sSolve = grove . fmap snd . (!!! 10) . iterate (go 0) . Seq.fromList . zip [0..] . map (*magic)
    }
  where
    grove xs = do
        i <- Seq.elemIndexL 0 xs
        let ixs = (`mod` n) . (+ i) <$> [1000,2000,3000]
        vs <- traverse (`Seq.lookup` xs) ixs
        pure $ sum vs
      where
        n = Seq.length xs
    go :: Int -> Seq.Seq (Int, Int) -> Seq.Seq (Int, Int)
    go i xs = case postXsMaybe of
        (_, v) Seq.:<| postXs ->
          let newIx = (Seq.length preXs + v) `mod` (n-1)
          in  go (i+1) $ Seq.insertAt newIx (i,v) (preXs <> postXs)
        _ -> xs
      where
        n = Seq.length xs
        (preXs, postXsMaybe) = Seq.spanl ((/= i) . fst) xs
