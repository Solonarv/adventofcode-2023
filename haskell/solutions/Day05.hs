{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import AOC.Solution
import ParsingPrelude
-- import Util

import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Interval ((<=..<=), Extended(..))
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IntervalSet
import qualified Data.Interval as Interval

solution :: Solution ([Int], [MapSection]) Int Int
solution = Solution
  { decodeInput = (,)
      <$> do
            "seeds: "
            nums <- decimal `sepBy` hspace1
            eol; eol
            pure nums
      <*> mapSectionP `sepBy` eol
  , solveA = defSolver
  { solve = uncurry (lowestLocationNumber . asPoints)
  }
  , solveB = defSolver
  { solve = uncurry (lowestLocationNumber . asPairs)
  }
  , tests =
    [ unlines
      [ "seeds: 79 14 55 13"
      , ""
      , "seed-to-soil map:"
      , "50 98 2"
      , "52 50 48"
      , ""
      , "soil-to-fertilizer map:"
      , "0 15 37"
      , "37 52 2"
      , "39 0 15"
      , ""
      , "fertilizer-to-water map:"
      , "49 53 8"
      , "0 11 42"
      , "42 0 7"
      , "57 7 4"
      , ""
      , "water-to-light map:"
      , "88 18 7"
      , "18 25 70"
      , ""
      , "light-to-temperature map:"
      , "45 77 23"
      , "81 45 19"
      , "68 64 13"
      , ""
      , "temperature-to-humidity map:"
      , "0 69 1"
      , "1 0 69"
      , ""
      , "humidity-to-location map:"
      , "60 56 37"
      , "56 93 4"
      ] :=> [(PartA, "35"), (PartB, "46")]
    ]
  }

data MapSection = MapSection
  { msSource, msDest :: String
  , msRanges :: [Range]
  }

data Range = Range
  { rangeSourceStart, rangeDestStart, rangeLength :: Int
  }

mapSectionP :: Parser MapSection
mapSectionP = do
  from <- some letterChar
  "-to-"
  to <- some letterChar
  " map:"; eol
  ranges <- rangeP `sepEndBy` eol
  pure MapSection { msSource = from, msDest = to, msRanges = ranges }

rangeP :: Parser Range
rangeP = do
  destStart <- decimal
  hspace1
  srcStart <- decimal
  hspace1
  len <- decimal
  pure Range { rangeSourceStart = srcStart, rangeDestStart = destStart, rangeLength = len }

characteristics :: IntervalSet Int -> [MapSection] -> Map String (IntervalSet Int)
characteristics seeds = go (Map.singleton "seed" seeds)
  where
    go !acc [] = acc
    go !acc (sec:secs) = go (applySec sec acc) secs
    applySec (MapSection src dest ranges) acc = case Map.lookup src acc of
      Nothing -> acc
      Just prev -> Map.insert dest (translateRanges ranges prev) acc

translateRanges :: [Range] -> IntervalSet Int -> IntervalSet Int
translateRanges ranges orig = let
    rangeIntervals = [(Finite s <=..<= Finite (s+l-1), d-s) | Range s d l <- ranges]
    intervalsToRemove = [(IntervalSet.intersection orig (IntervalSet.singleton ri), d) | (ri, d) <- rangeIntervals]
    intervalsToAdd = [shiftIntervalSet d i | (i, d) <- intervalsToRemove]
    removals = IntervalSet.unions (fst <$> intervalsToRemove)
    adds = IntervalSet.unions intervalsToAdd
  in IntervalSet.union adds (IntervalSet.difference orig removals)

shiftIntervalSet :: Int -> IntervalSet Int -> IntervalSet Int
shiftIntervalSet d = IntervalSet.fromAscList . fmap (Interval.mapMonotonic (+d)) . IntervalSet.toAscList

lowestLocationNumber :: IntervalSet Int -> [MapSection] -> Maybe Int
lowestLocationNumber seeds almanac = case Map.lookup "location" (characteristics seeds almanac) of
  Nothing -> Nothing
  Just locSet -> case IntervalSet.toAscList locSet of
    [] -> Nothing
    interval:_ -> case Interval.lowerBound interval of
      Finite m -> Just m
      _ -> Nothing

asPairs :: [Int] -> IntervalSet Int
asPairs = IntervalSet.fromList . go
  where
    go (a:b:cs) = (Finite a <=..<= Finite (a+b-1)) : go cs
    go [_] = error "bad input to asPairs"
    go _ = []

asPoints :: [Int] -> IntervalSet Int
asPoints = IntervalSet.fromList . fmap Interval.singleton