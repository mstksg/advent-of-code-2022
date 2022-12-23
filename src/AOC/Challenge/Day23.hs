{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day23 (
    day23a
  , day23b
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

step :: Set Point -> Int -> Set Point
step xs n = S.fromList . toList $ flip M.mapWithKey proposalMap $ \p0 x ->
    if allProps M.! x > 1
      then p0
      else x
  where
    proposalMap = flip M.fromSet xs \p ->
      if S.null $ fullNeighbsSet p `S.intersection` xs
        then p
        else (+ p) . fromMaybe 0 . asum @_ @Maybe . take 4 . drop (n `mod` 4) . cycle $ [
            V2 0 (-1) <$ guard (S.null $ S.intersection xs (S.fromList [p+V2 0 (-1),p+V2 (-1) (-1),p+V2 1 (-1)]))
          , V2 0 1 <$ guard (S.null $ S.intersection xs (S.fromList [p+V2 0 1,p+V2 (-1) 1,p+V2 1 1]))
          , V2 (-1) 0  <$ guard (S.null $ S.intersection xs (S.fromList [p+V2 (-1) (-1),p+V2 (-1) 0,p+V2 (-1) 1]))
          , V2 1 0  <$ guard (S.null $ S.intersection xs (S.fromList [p+V2 1 (-1),p+V2 1 0,p+V2 1 1]))
          ]
    allProps = freqs $ proposalMap


day23a :: _ :~> _
day23a = MkSol
    { sParse = Just . parseAsciiSet (== '#')
    -- , sShow = ('\n':) . unlines . map (displayAsciiSet '.' '#')
    , sShow  = \xs -> show
        let Just (V2 (V2 xMin yMin) (V2 xMax yMax)) = boundingBox'  xs
            allPoints = S.fromList $ V2 <$> [xMin .. xMax] <*> [yMin .. yMax]
        in  ((xMax-xMin+1)*(yMax-yMin+1)) - S.size xs
        -- (S.size xs -) . S.size . S.intersection xs . S.fromList $ V2 <$> [xMin .. xMax] <*> [yMin .. yMax]
-- Returns @'V2' (V2 xMin yMin) (V2 xMax yMax)@.
-- -- | A version of 'boundingBox' that works for normal possibly-empty lists.
-- boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (V2 (g a))
-- boundingBox' = fmap boundingBox . NE.nonEmpty . toList

    -- , sSolve = \xs -> Just $ scanl step xs [0..9]
    , sSolve = \xs -> Just $ foldl' step xs [0..9]
    }

day23b :: _ :~> _
day23b = MkSol
    { sParse = sParse day23a
    , sShow  = show
    , sSolve = \xs -> fmap fst . firstRepeatedBy snd . zip [0..] $ scanl step xs [0..]
    }

-- firstRepeatedBy :: Ord a => (b -> a) -> [b] -> Maybe b

-- -- | Lazily find the first repeated item.
-- firstRepeated :: Ord a => [a] -> Maybe a
-- firstRepeated = firstRepeatedBy id
