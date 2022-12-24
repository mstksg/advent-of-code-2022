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

-- #.########################################################################################################################
-- #<><v<^<vv<><^vv><><v^.^>.^.>.<v^>v><v^.v^<v<vvvv.^^><>^vv<<<>>v>>^>v>>>v<.vv>v<.<v>><>^<<v^<<v^vv>^^<v<^<>v>^v<^v^..^^<<#
-- #<>^<<<^<<vv.<^.^>>>vv.v<<vv^...vvv><><>^<^vv.<v>>v><<^^vvv>v>>^>^<v>^<vv><vv<v^v^.vv<<>vv.^^<>v<<v^>..>v^><v^v<<^^<^.^v<#
-- #>^>>^<v<v^<v.>vv^>vv^<vv.<>>^vv.<^^>v^vv>v<<vv<>><<^>.v^^v>v<<v^.^v>v>.<^^vvv><^v.v.<v>^>v<<^v.<vv>^v^v^^<>.<<<v^^^>^^^<#
-- #>>v>^><^v<.<>>>>^<v>>v^^^><<^v^<>^><>.v<.>^vv<^^<v>.<vv>v^>.v<v<vv.^><^v>><<<v<vvv.v^^>^.^>v^>^><>^<<^>v<<v^v>^v^>><>v.<#
-- #>>vv<^.^<>v<v<^<.>^v^.<>>.v^^^.vv^v.<v<>v.>>>v>>.<<<<v<.^>^v<v^<v^<..^v<v^>^<v<>v<vv>.><<<>^>>>v>>>v^<^v>v^<..v>>>v>^>^>#
-- #>^^v>.^>^<v>^^v>>v<^<<>^>^^>v<.v^<<><v^>>>><>.><>>>vv<^v^v.<v<v>v^>><v><<><>>^v<^^<^v<v^>v^<>.<.^v>vv.v><<v.>.^>.^^^.<.>#
-- #<<v>><.>v<<vv>v.vv<><vv<>^>>>^v<<v>^<<<<v^<>>vv^^v^>.<.^><>v>>v<^v<.v^<^<>v^v^v^>^v^<>v<.>v<^^^.<><v.^^..^v>^>^><.>v>v.>#
-- #<^v>v>^.>>>^vv^<>><>^.<>^^<<<^<><.v>^>.>.vvv><v<<>v^<^^<>^vv<vvv<v<<v<^<<>><v<>vv^v<.^.>^>v<<^<><><<<^<.>.v^^.<>><v^<^<<#
-- #>.<^<v^^><<.v^v<>v<^vv>>vv>^>^<.>>^v<vv.>.v<>>v<^^<><^^^v.<.v>>^.v^^^>>v^<>.<v^v^.^^v^vv.>>>^^v^..<^v..<.v<>v^<vv.^>^>v>#
-- #>v^^v>^<v>>vv^v^>.<><>v^^v.^^>.<<.>^<^<v^.^<><v.v<<^^^^<v^v>.^v^<.v^<<^^.><>.<v.v<^^>^^>>>^v^^..vv<v><>^>.><v^^..^^<.<..#
-- #<^<.v>>vv>v<<v<^<>>v^<v>.v^<>vvv..<^>>v>v^v^<^><^<^^v>v>.>vv>^^^>><.<<>^^>.>.>v><^<^v^^<vv<>>^^v<<v>.^.v.<v.^.^v<<^>^<>>#
-- #<.^v>^>.>^^>^>^<>>>.>^^^vv^v<>v.<^vv.vv^>>>^^v>^>v>>vvv^><v.<^><<>^^.v.vv>>vv>^vv<<<^..v<^><<<>>^<<v^<^<<.>^^<><<^^<vv^>#
-- #<^><>>>>^.^^v>v>^^.^<>><.<><v<<^>v><^^>><^^v>^>^^><^^>^v^<<><<.v^v>>>vvv^v>v^.^>.>v^vv>>>v^>^^.>>.>>vvv<^<<vvvv<^>><v^v<#
-- #<v^<<>^^v<^^<v^<^v><>..^<^>v^^<^<v>v.>>v>><><>v^>^v<vv^v^vv<.^^>>.<<^v>^^<>><<^>v^<^>v>^<>>>v>^.^<.^.^<v>vv.^v>v<vvv^^^<#
-- #<>v<v^^v^v<<<v^^><v<>v<^<>v^v<^^>^<v<v^.>^<vv^>^^<>v<>>>.v^v.<<<<.v^v>^<>><.v<>^.<>^.^vv<>.^<v.^v<^>>>^<^v^<.<>>v>^.>>^>#
-- #>>^>>vv.>v^v>>.vv>^<^v^>>^^<<<>v<>>><^.<^>^<>^.>>.<^^.^<.v<^vv>><<..<<.<v^>^^<>.>.<>><>>v^>.v.^^>^vv<<^^v>v^>vv>^<^v.v>>#
-- #<v.<v^^^<^><<^>v^><<v.^v^^>vv><vvv>^^^<v>^>vv<^^^>.>><<^<^v<>.v><^><>v<<.<^vv>.^^^<v>v>>>v<^.v^>>>^>^<<..v.>vv.>>v>^<^^>#
-- #<vvv>^^<<vv.v><^..>v.<v<v<<<v^>v<<>.><v<>.<vv^><^<<^v^..>v<v^v>>>^>><v.>>^<>v<^^.vv>>v>.v^^^><^<>>>^<<<<<...v><v>v><^<v<#
-- #>v^>v^<v>>><<^v>>>v^<^v.^.v^v.<^<^>.<<.<<.^^vv<v>v>><^^.><>v.>>..^.<vv><>.v>.^^>^vvv>.v>.<.^<v<>^><^v<>>>.<^^v<<>>>>^v>.#
-- #<>^>.v^<<><.v^<^<.>^>..<<vv^v<>>v>vv.<><<>^>><^^<v.<v<^<v^>vv^^<>vv>^<^^<v^.>^<^>v<v><^v>v^<^.v^<<>v>.>vv^^v^>vv>^.v><^<#
-- #<>>^.v><^v>^><>v><>.v>vv>^v^^v>.^..>^<^<vv^vv>v<<<v<<v<>.<.>>^^v<^<>v>^<^<v.>^<v.^<^^vvv>.<>^><^v.^>>v<v>^^.^^v>v>vv<<<>#
-- #.>>^^>v.v^>v^^>vv<vv<v.vv>.vv<<<<^^<v.<v><vv.v<^<^v<.>..^>v^<^^.<>v^<v..^^<^v^^>^<^.v>v<^^>vvvvv.>>>^vv^^^<>.v>v^..>v^.>#
-- #>^>v^.^vv^v.vv>v.>v^<><^<>v^<^v><.v<^v>^v>.v<v>^vvvvv>.<vv>>^^<^^^<.<>v>.<>>.v<^<v<^.v.<><>v^v^^^v^>^^.^^<><><<<v^^<<^v<#
-- #<>>v.<<v<^<^<^.vv>>>>>v^..<><<v^>^^.>>^<.<>^v^<^>v<.vv<v<<^vv^.<v<<v^vv<<.v<>^v^<^v<^vv>>><v>vvv><><><.><v>^><<.>><<>^^.#
-- #.<^v><>v>^v^v^><^^.v<<>^<^<>v..^>vv>vvvv<<vv>>>><v<^<^v^>v>^<>^^v.>>v>^.v^vvv<><v^.><<vv.>^.^v<>>v.<vv>^<^>>>v^>><v.>>><#
-- ########################################################################################################################.#

parseChar '>' = Just East
parseChar '<' = Just West
parseChar '^' = Just South
parseChar 'v' = Just North
parseChar _ = Nothing

data MapState = MS
    { blizz :: !Int
    , pos   :: !Point
    }
  deriving stock (Eq, Show, Ord, Generic)

instance NFData MapState

day24a :: _ :~> _
day24a = MkSol
    { sParse = Just . parseAsciiMap parseChar
    , sShow  = show . fst
    , sSolve = \(M.mapKeys (subtract 1)->bmap) ->
        let Just (V2 _ maxes) = boundingBox' $ M.keys bmap
            goal = maxes + V2 0 1
            stepBs = zipWith (\d p ->
                        mod <$> (p + dirPoint d) <*> (maxes+1)
                    ) dirs
            cycleLength = product $ maxes + 1
            bHist = Seq.fromList . map S.fromList . take cycleLength $ iterate stepBs (M.keys bmap)
            dirs = toList bmap
            outerEdge = S.delete goal $ S.delete (V2 0 (-1)) $ S.fromList
                [ V2 x y
                | x <- [-2 .. view _x maxes+2]
                , y <- [-2 .. view _y maxes+2]
                , x < 0 || y < 0 || x > view _x maxes || y > view _y maxes
                ]
            expander (MS t p) = M.fromList
                [ (MS (t+1) p', 1)
                | p' <- S.toList cands
                ]
              where
                bs = bHist `Seq.index` ((t+1) `mod` cycleLength)
                cands = (S.fromList (p:cardinalNeighbs p) `S.difference` outerEdge) `S.difference` bs
        in  aStar
              (\(MS _ p) -> sum $ abs <$> (p - goal))
              expander
              (MS 0 (V2 0 (-1)))
              ((== goal) . pos)
    }

data Leg = LFirst | LSecond | LThird
  deriving stock (Eq, Show, Ord, Generic)

instance NFData Leg

data MapState2 = MS2
    { time2 :: !Int
    , leg2  :: !Leg
    , pos2  :: !Point
    }
  deriving stock (Eq, Show, Ord, Generic)

instance NFData MapState2

day24b :: _ :~> _
day24b = MkSol
    { sParse = Just . parseAsciiMap parseChar
    , sShow  = show . fst
    , sSolve = \(M.mapKeys (subtract 1)->bmap) ->
        let Just (V2 _ maxes) = boundingBox' $ M.keys bmap
            goal = maxes + V2 0 1
            origin = V2 0 (-1)
            stepBs = zipWith (\d p ->
                        mod <$> (p + dirPoint d) <*> (maxes+1)
                    ) dirs
            cycleLength = product $ maxes + 1
            bHist = Seq.fromList . map S.fromList . take cycleLength $ iterate stepBs (M.keys bmap)
            dirs = toList bmap
            outerEdge = S.delete goal $ S.delete origin $ S.fromList
                [ V2 x y
                | x <- [-2 .. view _x maxes+2]
                , y <- [-2 .. view _y maxes+2]
                , x < 0 || y < 0 || x > view _x maxes || y > view _y maxes
                ]
            totalLeg = sum $ abs <$> (goal - origin)
            heuristic (MS2 _ l p) = case l of
              LFirst  -> sum (abs <$> (p - goal)) + 2*totalLeg
              LSecond -> sum (abs <$> (p - origin)) + totalLeg
              LThird -> sum (abs <$> (p - goal))
            expander (MS2 t l p) = M.fromList
                [ (MS2 (t + 1) l' p', 1)
                | p' <- S.toList cands
                ]
              where
                bs = bHist `Seq.index` ((t+1) `mod` cycleLength)
                -- bs = stepBs bs
                cands = (S.fromList (p:cardinalNeighbs p) `S.difference` outerEdge) `S.difference` bs
                l' = case l of
                  LFirst | p == goal -> LSecond
                  LSecond | p == origin -> LThird
                  _ -> l
        in  aStar
              heuristic
              expander
              (MS2 0 LFirst origin)
              (\(MS2 _ l p) -> l == LThird && p == goal)
    }
