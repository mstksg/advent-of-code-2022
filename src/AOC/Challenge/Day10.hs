{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
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

data Instr = Noop | Addx Int
  deriving stock (Show, Eq, Ord, Generic)

instance NFData Instr

data CState = CState
        { csCycle   :: !Int
        , csCounter :: !Int
        }
  deriving stock (Show, Eq, Ord, Generic)

instance NFData CState

processInstr :: CState -> Instr -> (CState, Int)
processInstr (CState y o) = \case
    Noop   -> (CState (y+1) o    , 0)
    Addx i -> (CState (y+2) (o+i), 1)

runProgram :: [Instr] -> IntMap Int
runProgram = IM.fromList
           . map (\(CState y o) -> (y, o))
           . scanl' (\i -> fst . processInstr i) (CState 1 1)

runCrt :: [Instr] -> Set Point
runCrt = S.fromList
       . mapMaybe (\(p@(V2 x _), s) -> p <$ guard (abs (x - s) <= 1))
       . concatMap expand
       . snd . mapAccumL processInstr' (CState 1 1)
  where
    processInstr' s i = (s', (s', r))
      where
        (s', r) = processInstr s i
    expand (CState y o, r) =
        [ ( V2 px py, o )
        | z <- [y .. y + r]
        , let (py, px) = (z-1) `divMod` 40
        ]


parseInstr :: String -> Maybe Instr
parseInstr str = case words str of
    ["noop"]    -> Just Noop
    ["addx", n] -> Addx <$> readMaybe n
    _           -> Nothing

day10a :: _ :~> _
day10a = MkSol
    { sParse = traverse parseInstr . lines
    , sShow  = show
    , sSolve = \instrs ->
        let results = runProgram instrs
        in  fmap sum $ flip traverse [20,60,100,140,180,220] \i ->
              (* i) . snd <$> IM.lookupLE i results
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = sParse day10a
    , sShow  = ('\n':) . displayAsciiSet '.' '#'
    , sSolve = Just . runCrt
        -- let results = runCrt instrs
            -- crtMap = IM.fromList
            --   [
            --   ]
        -- in  Just (runCrt results)
        -- IM.fromList
        --     [ (i, if )
        --     | i <- [1..240]
        --     , Just spritePos <- [IM.lookupLE i results]
        --     ]

        -- in  fmap sum $ flip traverse [20,60,100,140,180,220] \i ->
        --       (* i) . snd <$> IM.lookupLE i results
    -- , sSolve = Just
    }














