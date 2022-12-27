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
  , stepForwardFrom
  , testSteps
  , testSteps2
  , testSteps3
  , testSteps4
  , testStats
  , MoveState(..)
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import           Data.Bitraversable (bitraverse)
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import           Data.Group
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

data Step = Turn Dir
          | Forward Int
  deriving stock (Show, Eq, Ord)

parseStep :: String -> Maybe [Step]
parseStep = evalStateT (many (Turn <$> stepTurn <|> Forward <$> stepForward))
  where
    stepTurn = (South <>) <$> StateT (bitraverse parseDir pure <=< uncons)
    stepForward = StateT (bitraverse readMaybe pure . span isDigit)

parseInput :: String -> Maybe (Int, Map Point Tile, [Step])
parseInput inp = do
    (rawMp, rawSteps) <- listTup $ splitOn "\n\n" inp
    let mp = parseAsciiMap identChar rawMp
    steps <- parseStep rawSteps
    gridSize <- minimumMay $ map (length . filter (not . isSpace)) (lines rawMp)
    pure (gridSize, mp, steps)
  where
    identChar '.' = Just Floor
    identChar '#' = Just Wall
    identChar _   = Nothing


-- bitraverse (pure . parseAsciiMap identChar) parseStep
--          <=< listTup . splitOn "\n\n"

data MoveState = MS { pos :: !Point, dir :: !Dir }
  deriving stock (Show, Eq, Ord)

step :: (MoveState -> MoveState) -> Map Point Tile -> MoveState -> Step -> MoveState
step wrapper mp (MS p d) = \case
    Turn e    -> MS p (d <> e)
    Forward n -> stepStraight n (MS p d)
  where
    stepStraight 0 ms = ms
    stepStraight !n ms@(MS q _)
        | mp M.! q' == Floor = stepStraight (n-1) nextStep
        | otherwise          = ms
      where
        tryNextStep = q + dirPoint d
        nextStep@(MS q' _)
          | tryNextStep `M.member` mp = MS tryNextStep d
          | otherwise                 = wrapper (MS tryNextStep d)


score :: MoveState -> Int
score (MS (V2 x y) d) = 1000 * (y+1) + 4 * (x+1) + dp
  where
    dp = case d of
      East -> 0
      South -> 3
      West -> 2
      North -> 1

solve
    :: (MoveState -> MoveState)
    -> Map Point Tile
    -> [Step]
    -> MoveState
solve wrapper mp = foldl' (step wrapper mp) s0
  where
    x0 = minimum [ x | V2 x y <- M.keys mp , y == 0 ]
    s0 = MS (V2 x0 0) East

day22a :: (Int, Map Point Tile, [Step]) :~> MoveState
day22a = MkSol
    { sParse = parseInput
    , sShow  = show . score
    , sSolve = \(_, mp, xs) -> Just $ solve (wrapper mp) mp xs
    }
  where
    wrapper mp (MS (V2 x y) e) = (`MS` e) case e of
      North -> V2 x (minimum [ y' | V2 x' y' <- M.keys mp, x' == x ])
      South -> V2 x (maximum [ y' | V2 x' y' <- M.keys mp, x' == x ])
      West  -> V2 (maximum [ x' | V2 x' y' <- M.keys mp, y' == y ]) y
      East  -> V2 (minimum [ x' | V2 x' y' <- M.keys mp, y' == y ]) y

day22b :: (Int, Map Point Tile, [Step]) :~> _
day22b = MkSol
    { sParse = parseInput
    -- , sShow  = show
    , sShow  = show . score
    -- , sSolve = \(g, _, _) -> Just g
    , sSolve = \(g, mp, xs) -> Just $ solve (wrapper g mp) mp xs
    }
  where
    wrapper g mp = traceShowId . fromJust . stepForwardFrom g (M.keysSet mp) 1 . stepBack
    stepBack (MS x d) = MS (x - dirPoint d) d

stepForwardFrom
    :: Int        -- ^ Grid size
    -> Set Point  -- ^ Valid points
    -> Int        -- ^ distance
    -> MoveState  -- ^ Origin
    -> Maybe MoveState  -- ^ End
stepForwardFrom g mp q = trace "go" . go 10 q
  where
    go :: Int -> Int -> MoveState -> Maybe MoveState
    go 0 _ _ = Nothing
    go r n ms0@(MS x0 d0)
        -- | x0 `S.notMember` mp = error "hey"
        | naiveStep `S.member` mp = pure $ MS naiveStep d0
        | otherwise               = traceShow (r, n, ms0, naiveStep, V3 turnDist boundDist overDist) $ do
            MS x1 d1 <- traceShowId $ go (r-1) (turnDist + 1 + overDist) (MS x0 (d0 <> East))
            MS x2 d2 <- traceShowId $ go (r-1) (turnDist + 1 + boundDist) (MS x1 (d1 <> West))
            pure $ MS x2 (d2 <> East)
      where
        naiveStep = x0 + n *^ dirPoint d0
        V3 turnDist boundDist overDist = case d0 of
          North -> V3 (negate (view _x x0+1) `mod` g) (negate (view _y x0+1) `mod` g) (view _y naiveStep `mod` g)
          South -> V3 (view _x x0 `mod` g)  (view _y x0 `mod` g) (negate (view _y naiveStep+1) `mod` g)
          East  -> V3 (view _y x0 `mod` g) (negate (view _x x0 + 1) `mod` g) (view _x naiveStep `mod` g)
          West  -> V3 (negate (view _y x0+1) `mod` g) (view _x x0 `mod` g) (negate (view _x naiveStep+1) `mod` g)
        -- V2 turnDist boundDist = (`mod` g) . negate <$> rotPoint (invert d0) (shiftNeg <$> x0)
        -- -- boundDist
        -- --   | boundDist' == 0 = g
        -- --   | otherwise       = boundDist'
        -- overDist = (view _y (rotPoint (invert d0) (shiftNeg <$> naiveStep))) `mod` g
    -- distToLine x
    --   | x < 0     = (x) `mod` g
    --   | otherwise = x `mod` g
    shiftNeg x
      | x < 0     = x + 1
      | otherwise = x
        -- V2 turnDist boundDist = (`mod` g) <$> rotPoint d0 x0
-- should be 8, 14

testStats :: Int -> Dir -> Int -> Point -> (Point, V3 Int, V3 Int, V3 Int)
testStats g d n x0 = (naiveStep, stats, (`mod` g) <$> stats, stats2)
  where
    naiveStep = x0 + n *^ dirPoint d
    V2 turnDist boundDist = negate <$> rotPoint (invert d) (shiftNeg <$> x0)
    overDist = (view _y (rotPoint (invert d) (shiftNeg <$> naiveStep)))
    stats = V3 turnDist boundDist overDist
    shiftNeg x
      | x < 0     = x + 1
      | otherwise = x
    stats2 = case d of
      North -> V3 (negate (view _x x0+1) `mod` g) (negate (view _y x0+1) `mod` g) (view _y naiveStep `mod` g)
      South -> V3 (view _x x0 `mod` g)  (view _y x0 `mod` g) (negate (view _y naiveStep+1) `mod` g)
      East  -> V3 (view _y x0 `mod` g) (negate (view _x x0 + 1) `mod` g) (view _x naiveStep `mod` g)
      West  -> V3 (negate (view _y x0+1) `mod` g) (view _x x0 `mod` g) (negate (view _x naiveStep+1) `mod` g)
    -- distToLine x
    --   | x < 0     = (x) `mod` g
    --   | otherwise = x `mod` g

testSteps :: Set Point
testSteps = parseAsciiSet (/= ' ') . unlines $
  ["............"
  ,"............"
  ,"............"
  ,"............"
  ,"............"
  ,"............"
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "]


-- ............
-- .......EJ...
-- .......DI...
-- .......CH...
-- .......BG...
-- .......AF...
-- ......
-- ...x..abcde
-- ...y..fghij
-- ......
-- ......
-- ......

testSteps2 :: Set Point
testSteps2 = parseAsciiSet (/= ' ') . unlines $
  ["............"
  ,"............"
  ,"............"
  ,"............"
  ,"............"
  ,"............"
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "]


-- -............
-- |............
-- |............
-- |.......JIHGF
-- |.......EDCBA
-- -............
-- -......
-- |......
-- |......
-- |......
-- |......
-- -......
-- -......
-- |...x..abcde
-- |...y..fghij
-- |......
-- |......
-- -......

testSteps3 :: Set Point
testSteps3 = parseAsciiSet (/= ' ') . unlines $
  ["............"
  ,"............"
  ,"............"
  ,"............"
  ,"............"
  ,"............"
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "
  ,"......      "]


testSteps4 :: Set Point
testSteps4 = parseAsciiSet (/= ' ') . unlines $
  ["            ......      "
  ,"            ......      "
  ,"            ......      "
  ,"            ......      "
  ,"            ......      "
  ,"            ......      "
  ,"..................      "
  ,"..................      "
  ,"..................      "
  ,"..................      "
  ,"..................      "
  ,"..................      "
  ,"            ............"
  ,"            ............"
  ,"            ............"
  ,"            ............"
  ,"            ............"
  ,"            ............"]


--       %%%%%%############
--       %%%%%%############
--       %%%%Q%############
--       %%%%%%############
--       %%%%%%###q#######a
--       %%%%%%############
--       %%%%%%......      
--       %%%%%%......      
--       %%%%%%......      
--       %%%%%%......      
--       %%%%%%......      
--       %%%%r%...... A    
-- ..................      
-- ...............x..abcde 
-- ...............y..fghij
-- ..................      
-- ..................      
-- ..................      
--             .........FA.
--             .........GB.
--             .........HC.
--             .........ID.
--             .........JE.
--             ............
