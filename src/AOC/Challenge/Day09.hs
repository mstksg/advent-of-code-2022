-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Common (strictIterate, (!!!), listTup)
import           AOC.Common.Point (Point, Dir, dirPoint, parseDir)
import           AOC.Solver ((:~>)(..))
import           Control.Monad ((<=<))
import           Data.Bitraversable (bitraverse)
import           Control.Arrow ((<<<))
import           Data.List (scanl')
import           Data.Maybe (listToMaybe)
import           Text.Read (readMaybe)
import qualified Data.Set as S
import qualified Control.Scanl as Scan
import qualified Control.Foldl as Fold
import           Control.Monad.State

sumScan :: Num a => Scan.Scan a a
sumScan = Scan.Scan (\x -> state \y -> let !z = x+y in (z, z)) 0

followScan :: Scan.Scan Point Point
followScan = Scan.Scan (\x -> state (\y -> let !z = go y x in (z,z))) 0
  where
    go curr new
      | maximum (fmap abs dist) > 1 = curr + signum dist
      | otherwise                   = curr
      where
        dist = new - curr


parseLine :: String -> Maybe (Dir, Int)
parseLine = bitraverse (parseDir <=< listToMaybe) readMaybe
        <=< listTup . words

lag :: [Point] -> [Point]
lag = scanl' go 0
  where
    go curr new
      | maximum (fmap abs dist) > 1 = curr + signum dist
      | otherwise                   = curr
      where
        dist = new - curr

day09 :: Int -> [(Dir, Int)] :~> Int
day09 lagAmount = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = Just . S.size . S.fromList
             . Scan.scan scanner
             -- . (!!! lagAmount)
             -- . strictIterate ()
             -- . scanl' (+) 0
             . concatMap (\(d, i) -> replicate i (dirPoint d))
    }
  where
    scanner = foldr (<<<) sumScan $ replicate lagAmount followScan

day09a :: [(Dir, Int)] :~> Int
day09a = day09 1

day09b :: [(Dir, Int)] :~> Int
day09b = day09 9
