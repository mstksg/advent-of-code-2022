{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import           Data.Bitraversable (bitraverse)
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

data Tile = Floor | Wall
  deriving stock (Show, Eq, Ord)

identChar :: Char -> Maybe Tile
identChar '.' = Just Floor
identChar '#' = Just Wall
identChar _   = Nothing

data Step = Turn Dir
          | Forward Int
  deriving stock (Show, Eq, Ord)

identStep :: String -> Maybe [Step]
identStep = go
  where
    go xs = case span isDigit xs of
      (p, r:rest) -> do
        s  <- readMaybe p
        d  <- case r of
          'L' -> pure East
          'R' -> pure West
          _   -> empty
        ys <- go rest
        pure $ Forward s : Turn d : ys
      (p, _) -> (:[]) . Forward <$> readMaybe p

parseInput :: String -> Maybe (Map Point Tile, [Step])
parseInput = bitraverse (pure . parseAsciiMap identChar) identStep
         <=< listTup . splitOn "\n\n"

data MoveState = MS { pos :: Point, dir :: Dir }
  deriving stock (Show, Eq, Ord)

step :: Map Point Tile -> MoveState -> Step -> MoveState
step mp (MS p d) = \case
    Turn e    -> MS p (d <> e)
    Forward n -> MS (stepStraight n p) d
  where
    stepStraight !n q
        | n <= 0    = q
        | otherwise = case mp M.! nextStep of
            Floor -> stepStraight (n-1) nextStep
            Wall  -> q
      where
        nextStep = wrap (q + dirPoint d)
    wrap q@(V2 x y)
      | q `M.member` mp = q
      | otherwise       = case d of
          North -> V2 x (minimum [ y' | V2 x' y' <- M.keys mp, x' == x ])
          South -> V2 x (maximum [ y' | V2 x' y' <- M.keys mp, x' == x ])
          West  -> V2 (maximum [ x' | V2 x' y' <- M.keys mp, y' == y ]) y
          East  -> V2 (minimum [ x' | V2 x' y' <- M.keys mp, y' == y ]) y

score :: MoveState -> Int
score (MS (V2 x y) d) = 1000 * (y+1) + 4 * (x+1) + dp
  where
    dp = case d of
      East -> 0
      South -> 3
      West -> 2
      North -> 1

day22a :: _ :~> _
day22a = MkSol
    { sParse = parseInput
    , sShow  = show . score
    , sSolve = \(mp, xs) ->
        let x0 = minimum [ x | V2 x y <- M.keys mp , y == 0 ]
            s0 = MS (V2 x0 0) East
        in  Just $ foldl' (step mp) s0 xs
    }

day22b :: _ :~> _
day22b = MkSol
    { sParse = sParse day22a
    , sShow  = show
    , sSolve = Just
    }

