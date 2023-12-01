{-# LANGUAGE OverloadedStrings #-}
module Day01 where

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [[Maybe DigitEntry]] Int Int
solution = Solution
  { decodeInput = many digitEntryP  `sepBy` space1
  , solveA = defSolver
  { solve = Just . \ls -> sum [head l * 10 + last l | l <- mapMaybe (>>= realOnly) <$> ls]
  }
  , solveB = defSolver
  { solve = Just . \ls -> sum [head l * 10 + last l | l <- mapMaybe (fmap digitValue) <$> ls]
  }
  , tests =
    [ "1abc2 pqr3stu8vwx a1b2c3d4e5f treb7uchet" :=> [(PartA, "142")]
    , "two1nine eightwothree abcone2threexyz xtwone3four 4nineeightseven2 zoneight234 7pqrstsixteen"
      :=> [(PartB, "281")]
    ]
  }

data DigitEntry = RealDigit Int | WordDigit Int
  deriving (Eq, Show, Ord)

digitEntryP :: Parser (Maybe DigitEntry)
digitEntryP = (Just . RealDigit <$> singleDigit 10)
  <|> asum (fmap (Just . WordDigit) . onecharify <$>
  [ 0 <$ "zero"
  , 1 <$ "one"
  , 2 <$ "two"
  , 3 <$ "three"
  , 4 <$ "four"
  , 5 <$ "five"
  , 6 <$ "six"
  , 7 <$ "seven"
  , 8 <$ "eight"
  , 9 <$ "nine"
  ]) <|> (Nothing <$ alphaNumChar)

realOnly :: DigitEntry -> Maybe Int
realOnly (RealDigit d) = Just d
realOnly _ = Nothing

digitValue :: DigitEntry -> Int
digitValue (RealDigit d) = d
digitValue (WordDigit d) = d

firstLastVal :: [Int] -> Int
firstLastVal [] = 0
firstLastVal xs = 10 * head xs + last xs

reassemble :: [Maybe DigitEntry] -> String
reassemble = concatMap showDE
  where
    showDE Nothing = "_"
    showDE (Just (RealDigit d)) = show d
    showDE (Just (WordDigit d)) = case d of
      0 -> "zero"
      1 -> "one"
      2 -> "two"
      3 -> "three"
      4 -> "four"
      5 -> "five"
      6 -> "six"
      7 -> "seven"
      8 -> "eight"
      9 -> "nine"
      _ -> "<??>"