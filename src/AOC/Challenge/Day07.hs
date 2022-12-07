{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
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

data INode = Dir Filesystem
           | File String Int
  deriving stock (Eq, Show, Ord, Generic)

instance NFData INode

type Filesystem = Map String INode

data CDDest = Root | GoUp | GoDown String
  deriving stock (Eq, Show, Ord, Generic)

data LSVal = LSDir String | LSFile String Int
  deriving stock (Eq, Show, Ord, Generic)

instance NFData LSVal
instance NFData CDDest

data Instr = CD CDDest
           | LS [LSVal]
  deriving stock (Eq, Show, Ord, Generic)

instance NFData Instr

parseInstrs :: [String] -> [Instr]
parseInstrs = go
  where
    go [] = []
    go (x:xs) = case words x of
      "$":"cd":d:[] -> case d of
        "/" -> CD Root : go xs
        ".." -> CD GoUp : go xs
        _ -> CD (GoDown d) : go xs
      "$":"ls":[] ->
        let (fs, ys) = eatFiles xs
        in  LS fs : go ys
    eatFiles = \case
      [] -> ([],[])
      x:xs -> case words x of
        "dir":d:[] -> first (LSDir d :) $ eatFiles xs
        "$":_ -> ([], x:xs)
        n:fl:[] -> first (LSFile fl (read n):) $ eatFiles xs

buildFlat :: [Instr] -> [([String],String,Int)]
buildFlat = map (over _1 reverse) . go []
  where
    go :: [String] -> [Instr] -> [([String], String, Int)]
    go currDir = \case
      [] -> []
      CD Root:xs -> go [] xs
      CD GoUp:xs -> go (tail currDir) xs
      CD (GoDown d):xs -> go (d:currDir) xs
      LS fs:xs -> mapMaybe
        (\case LSDir _ -> Nothing
               LSFile f i -> Just (currDir, f, i)
        )
        fs ++ go currDir xs

buildSizes
    :: [([String],String,Int)]
    -> Map [String] Int
buildSizes = foldl' go M.empty
  where
    go mp (dir, f, i) = M.unionWith (+) mp $
      M.fromList $ map (\ds -> (ds, i)) $ inits dir

buildLeafSizes
    :: [([String],String,Int)]
    -> Map [String] Int
buildLeafSizes = foldl' go M.empty
  where
    go mp (dir, f, i) = M.insertWith (+) dir i  mp 

-- leafSizesToSizes
--     :: Map [String] Int
--     -> Map [String] Int
-- leafSizesToSizes = M.fromListWith (+) . concatMap f . M.toList
--   where
--     f (ds, i) = (,i) <$> inits ds

day07a :: _ :~> _
day07a = MkSol
    { sParse = Just . parseInstrs . lines
    , sShow  = show
    , sSolve = Just . sum . M.filter (<= 100000) . buildSizes . buildFlat
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = sParse day07a
    , sShow  = show
    , sSolve = \xs ->
        let sizes = sort . toList . buildSizes . buildFlat $ xs
        in  firstJust (go (last sizes)) $ sizes
    -- , sSolve = \xs -> 
        -- let leafSizes = buildLeafSizes $ buildFlat xs
        --     goodEnough = flip M.filterWithKey leafSizes $ \k _ ->
        --       sum (M.delete k leafSizes)
            -- allDirs = M.keysSet leafSizes
            -- withDeletedDir d = filter (\(dd,_,_) -> not $ d `isPrefixOf` dd 
            --     ) flats
            -- enough
        -- in  Just goodEnough
    }
  where
    go tot i = i <$ guard (tot - i <= (70000000 - 30000000)) 
