{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import           Data.Finite
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import           GHC.TypeLits
import           Data.Proxy
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.Map.NonEmpty              as NEM
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

parseChar :: Char -> Maybe Dir
parseChar '>' = Just East
parseChar '<' = Just West
parseChar '^' = Just South
parseChar 'v' = Just North
parseChar _   = Nothing

data MapState n = MS
    { time :: !Int
    , leg  :: !(Maybe (Finite n))  -- ^ Just n is on leg #n, Nothing is done
    , pos  :: !Point
    }
  deriving stock (Eq, Show, Ord, Generic)

instance NFData (MapState n)

-- | Assume corner at (0,0).
solve :: forall n. KnownNat n => NEMap Point Dir -> Maybe (Int, [MapState n])
solve bmap = first (subtract 1) <$> aStar
              heuristic
              expander
              (MS 0 (Just 0) origin)
              (\(MS _ l _) -> isNothing l)
  where
    V2 _ maxes  = boundingBox $ NEM.keys bmap
    cyclePeriod = product (maxes + 1)
    goal     = maxes + V2 0 1
    origin   = V2 0 (-1)
    stepBs   = zipWith (\d p -> mod <$> (p + dirPoint d) <*> (maxes + 1))
                       (toList bmap)
    bHist    = Seq.fromList . map S.fromList . take cyclePeriod
             $ iterate stepBs (toList (NEM.keys bmap))
    validPos = S.insert goal . S.insert origin . S.fromList $
                  sequence $ enumFromTo <$> 0 <*> maxes
    totalLegDist = mannDist goal origin
    destForLeg l
      | even (getFinite l) = goal
      | otherwise          = origin
    heuristic = \case
      MS _ Nothing  _ -> 0
      MS _ (Just l) p ->
        let legsLeft = fromInteger (natVal (Proxy @n)  - getFinite l)
        in  mannDist p (destForLeg l) + legsLeft * totalLegDist
    expander = \case
      MS _ Nothing _ -> M.empty
      MS t (Just l) p ->
        let bs = bHist `Seq.index` ((t+1) `mod` cyclePeriod)
            cands = (S.fromList (p:cardinalNeighbs p) `S.intersection` validPos) `S.difference` bs
            l'   | p == destForLeg l = strengthen (shift l)
                 | otherwise      = Just l
        in  M.fromList [ (MS (t + 1) l' p', 1) | p' <- S.toList cands ]


day24 :: forall n. KnownNat n => NEMap Point Dir :~> Int
day24 = MkSol
    { sParse = NEM.nonEmptyMap . parseAsciiMap parseChar
    , sShow  = show
    , sSolve = fmap fst . solve @n . NEM.mapKeys (subtract 1)
    }

day24a :: NEMap Point Dir :~> Int
day24a = day24 @1

day24b :: NEMap Point Dir :~> Int
day24b = day24 @3
