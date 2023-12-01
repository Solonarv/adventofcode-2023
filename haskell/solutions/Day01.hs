module Day01 where

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [[Int]] Int Int
solution = Solution
  { decodeInput = (mapMaybe (char2digitBase 10) <$> some alphaNumChar)  `sepBy` space1
  , solveA = defSolver
  { solve = Just . \ls -> sum [head l * 10 + last l | l <- ls]
  }
  , solveB = defSolver
  , tests =
    [ "1abc2 pqr3stu8vwx a1b2c3d4e5f treb7uchet" :=> [(PartA, "142")]
    ]
  }