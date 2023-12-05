{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Semigroup (Min(..))

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

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
  { solve = uncurry lowestLocationNumber
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

rangesToMap :: [Range] -> Int -> Int
rangesToMap [] x = x
rangesToMap (r:rs) x = fromMaybe (rangesToMap rs x) (maybeApplyRange r x)

maybeApplyRange :: Range -> Int -> Maybe Int
maybeApplyRange r x
  | x >= rangeSourceStart r
  , x <= rangeSourceStart r + rangeLength r
    = Just (x - rangeSourceStart r + rangeDestStart r)
  | otherwise = Nothing

seedCharacteristics :: Int -> [MapSection] -> Map String Int
seedCharacteristics sn almanac = Map.fromList knot
  where
    knot = -- we hope that the maps in the input contain no forward references, else this will hang
      ("seed", sn) :
      [ (dest, val)
      | MapSection src dest ranges <- almanac
      , Just orig <- [Prelude.lookup src knot]
      , let val = rangesToMap ranges orig
      ]

lowestLocationNumber :: [Int] -> [MapSection] -> Maybe Int
lowestLocationNumber seeds almanac = getMin
  <$> flip foldMap seeds \s ->
      Min <$> Map.lookup "location" (seedCharacteristics s almanac)

asPairs :: [Int] -> [Int]
asPairs (a:b:cs) = [a .. a+b-1] ++ asPairs cs
asPairs [_] = error "bad input to asPairs"
asPairs _ = []