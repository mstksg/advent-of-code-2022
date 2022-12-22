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

-- parseAsciiMap
--     :: (Char -> Maybe a)
--     -> String
--     -> Map Point a
-- parseAsciiMap f = toMapOf (asciiGrid <. folding f)

identChar :: Char -> Maybe Bool
identChar '.' = Just False
identChar '#' = Just True
identChar _   = Nothing

identStep :: String -> [Either Dir Int]
identStep = go
  where
    go xs = case span isDigit xs of
      (p, 'L':rest) -> Right (read p) : Left East : go rest
      (p, 'R':rest) -> Right (read p) : Left West : go rest
      (p,_) -> Right (read p) : []

score (d, V2 x y) = 1000 * (y+1) + 4 * (x+1) + dp
  where
    dp = case d of
      East -> 0
      South -> 3
      West -> 2
      North -> 1

day22a :: _ :~> _
day22a = MkSol
    { sParse = fmap (bimap (parseAsciiMap identChar) identStep) . listTup . splitOn "\n\n"
    -- , sShow  = show
    , sShow  = show . score
    , sSolve = \(mp, xs) ->
        let x0 = minimum
              [ x
              | V2 x y <- M.keys mp
              , y == 0
              ]
            s0 = (East, V2 x0 0)
        in  Just $ foldl' (step mp) s0 xs
        -- let Just (V2 (V2 xMin yMin) (V2 xMax yMax)) = boundingBox' $ M.keysSet mp
        -- in  Just yMin
-- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
        -- let st0 = (East, )
-- boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
-- boundingBox = (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
--             . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))
    }
-- To finish providing the password to this strange input device, you need
-- to determine numbers for your final *row*, *column*, and *facing* as
-- your final position appears from the perspective of the original map.
-- Rows start from `1` at the top and count downward; columns start from
-- `1` at the left and count rightward. (In the above example, row 1,
-- column 1 refers to the empty space with no tile on it in the top-left
-- corner.) Facing is `0` for right (`>`), `1` for down (`v`), `2` for left
-- (`<`), and `3` for up (`^`). The *final password* is the sum of 1000
-- times the row, 4 times the column, and the facing.

-- In the above example, the final row is `6`, the final column is `8`, and
-- the final facing is `0`. So, the final password is 1000 \* 6 + 4 \* 8 +
-- 0: *`6032`*.
  where
    step mp (id->(d, p)) = \case
        Left q -> (d <> q, p)
        Right i -> (d, iterate stepStraight p !! i)
      where
        stepStraight r = case M.lookup nextPosPreBlock mp of
            Just False -> nextPosPreBlock
            Just True  -> r
            Nothing -> error "what"
          where
            nextPosPreWrap@(V2 x y) = r + dirPoint d
            nextPosPreBlock = case M.lookup nextPosPreWrap mp of
              Nothing    -> case d of
                North -> V2 x (minimum [ y' | V2 x' y' <- M.keys mp, x' == x ])
                South -> V2 x (maximum [ y' | V2 x' y' <- M.keys mp, x' == x ])
                West  -> V2 (maximum [ x' | V2 x' y' <- M.keys mp, y' == y ]) y
                East  -> V2 (minimum [ x' | V2 x' y' <- M.keys mp, y' == y ]) y
              Just _ -> nextPosPreWrap

day22b :: _ :~> _
day22b = MkSol
    { sParse = sParse day22a
    , sShow  = show
    , sSolve = Just
    }

