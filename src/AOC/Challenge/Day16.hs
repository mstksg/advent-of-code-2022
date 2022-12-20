{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day16 (
    day16a
  , day16b
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

-- Valve NA has flow rate=0; tunnels lead to valves MU, PH
-- Valve NW has flow rate=0; tunnels lead to valves KB, MH

parseLine :: String -> Maybe (String, (Int, Set String))
parseLine xs = do
    (a, bs) <- uncons $ words $ clearOut (not . isUpper) (tail xs)
    r       <- readMaybe $ clearOut (not . isDigit) xs
    pure (a, (r, S.fromList bs))

data PuzzState = PuzzState
    { time      :: !Int
    , currPos   :: !String
    , openPipes :: !(Set String)
    }
  deriving stock (Show, Generic, Eq, Ord)

instance NFData PuzzState

searchPuzzle
    :: Map String (Int, Set String)
    -> Maybe (Int, [PuzzState])
searchPuzzle mp = fmap (first reCost) $ aStar
    (oneTickCost . openPipes)
    expand
    (PuzzState 1 "AA" S.empty)
    (\(PuzzState t _ o) -> t >= 30 || S.size o >= S.size pipesWithFlow)
  where
    pipesWithFlow = M.keysSet $ M.filter ((> 0) . fst) mp
    reCost x = (oneTickCost S.empty * 30) - x
    oneTickCost :: Set String -> Int
    oneTickCost opened = sum . map fst . toList $ mp `M.withoutKeys` opened
    expand (PuzzState t p o) = M.fromList $ map (,newCost) (stayHereAndOpen ++ moveToAnother)
      where
        newCost = oneTickCost o
        moveToAnother = do
          p' <- toList $ snd (mp M.! p)
          pure (PuzzState (t+1) p' o)
        stayHereAndOpen = do
          guard $ p `S.member` pipesWithFlow && p `S.notMember` o
          pure $ PuzzState (t+1) p (S.insert p o)

puzzleFlow :: Map String (Int, Set String) -> [PuzzState] -> Int
puzzleFlow mp xs = sum . take 30 $ map flow (xs ++ repeat (last xs))
  where
    flow (PuzzState _ _ o) = sum . map fst . toList $ mp `M.restrictKeys` o

    
day16a :: _ :~> _
day16a = MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow  = show
    , sSolve = fmap fst . searchPuzzle
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = sParse day16a
    , sShow  = show
    , sSolve = Just
    }
