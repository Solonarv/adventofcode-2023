{-# LANGUAGE OverloadedStrings #-}
module Day06 where

import AOC.Solution
import ParsingPrelude
-- import Util

-- import Debug.Trace

solution :: Solution ([Int], [Int]) Int Int
solution = Solution
  { decodeInput = do
      "Time:"; hspace1
      times <- decimal `sepBy` hspace
      eol; "Distance:"; hspace1
      dists <- decimal `sepBy` hspace
      pure $ (times, dists)
  , solveA = defSolver
  { solve = Just . product . fmap recordBreakerCount . uncurry zip
  }
  , solveB = defSolver
  { solve = Just . recordBreakerCount . combineNumbers
  }
  , tests =
    [ "Time:      7  15   30\nDistance:  9  40  200"
      :=> [(PartA, "288"), (PartB, "71503")]
    ]
  }

-- system of inequalities for part 1:
--   0 < t < T; (T-t)t ≥ D;
-- the second inequality is a quadratic with leading coeff -1
-- and maximum at T/2.
-- solving -t² + Tt - D = 0 gives: t² - Tt + D = 0
--   t = ½(T ± √(T²-4D))
-- this interval contains all the solutions already
recordBreakerCount :: (Int, Int) -> Int
recordBreakerCount (rT, rD) = let
  fT, fD :: Double
  fT = fromIntegral rT
  fD = fromIntegral rD
  rtdet = sqrt (fT*fT - 4*fD)
  tMin = floor $ (fT-rtdet)/2 + 1
  tMax = ceiling $ (fT+rtdet)/2 - 1
  eqnType = (rT*rT - 4*rD) `compare` 0
  in case eqnType of
    LT -> 0
    _ ->tMax - tMin + 1

combineNumbers :: ([Int], [Int]) -> (Int, Int)
combineNumbers (times, dists) = (catInts times, catInts dists)
  where catInts = read . concatMap show