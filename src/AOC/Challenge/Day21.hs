{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day21 (
    day21a
  , day21b
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

data Node = Add String String
          | Mul String String
          | Div String String
          | Sub String String
          | Leaf Int
  deriving stock (Show)

parseLine :: String -> Maybe (String, Node)
parseLine str = case splitOn ":" str of
    [k, v] -> (k,) <$> case readMaybe v of
      Just x -> Just $ Leaf x
      Nothing -> case words v of
        [a,"+",b] -> Just $ Add a b
        [a,"-",b] -> Just $ Sub a b
        [a,"*",b] -> Just $ Mul a b
        [a,"/",b] -> Just $ Div a b
        _ -> Nothing
    _ -> Nothing


day21a :: _ :~> _
day21a = MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow  = show
    , sSolve = \xs -> Just
        let res = flip fmap xs \case
              Leaf i -> i
              Add a b -> res M.! a + res M.! b
              Mul a b -> res M.! a * res M.! b
              Div a b -> res M.! a `div` res M.! b
              Sub a b -> res M.! a - res M.! b
        in  res M.! "root"
    }

day21b :: _ :~> _
day21b = MkSol
    { sParse = sParse day21a
    , sShow  = show
    , sSolve = \xs ->
        let Add a b = xs M.! "root"
            hasHumn = flip M.mapWithKey xs \case
                "humn" -> const True
                _      -> \case
                  Leaf i -> False
                  Add a b -> hasHumn M.! a || hasHumn M.! b
                  Mul a b -> hasHumn M.! a || hasHumn M.! b
                  Div a b -> hasHumn M.! a || hasHumn M.! b
                  Sub a b -> hasHumn M.! a || hasHumn M.! b
            res = flip fmap xs \case
              Leaf i -> i
              Add a b -> res M.! a + res M.! b
              Mul a b -> res M.! a * res M.! b
              Div a b -> res M.! a `div` res M.! b
              Sub a b -> res M.! a - res M.! b
            humnEqual x target = case x of
              "humn" -> target
              _      -> case xs M.! x of
                Leaf i -> undefined
                Add a b
                  | hasHumn M.! a -> humnEqual a (target - res M.! b)
                  | otherwise     -> humnEqual b (target - res M.! a)
                Sub a b
                  | hasHumn M.! a -> humnEqual a (target + res M.! b)
                  | otherwise     -> humnEqual b (res M.! a - target)
                Mul a b
                  | hasHumn M.! a -> humnEqual a (target `div` res M.! b)
                  | otherwise     -> humnEqual b (target `div` res M.! a)
                Div a b
                  | hasHumn M.! a -> humnEqual a (target * res M.! b)
                  | otherwise     -> humnEqual b (res M.! a `div` target)
        in  Just $ humnEqual a (res M.! b)
        -- in  Just (hasHumn M.! a, hasHumn M.! b)

--             go i = res M.! a == res M.! b
--               where
--                 res = flip fmap xs \case
--                   Leaf i -> i
--                   Add a b -> res M.! a + res M.! b
--                   Mul a b -> res M.! a * res M.! b
--                   Div a b -> res M.! a `div` res M.! b
--                   Sub a b -> res M.! a - res M.! b
--                 ys = M.insert "humn" (Leaf i) xs
--         in  find go [0..]
    }
