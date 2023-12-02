{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Semigroup
import Linear.V3
import Linear.Vector

solution :: Solution [Game] Int Word
solution = Solution
  { decodeInput = gameP `sepBy1` eol
  , solveA = defSolver
    { solve = Just . sum .  fmap gameId . filter (isGamePossible (Reveal (V3 12 13 14)))
    }
  , solveB = defSolver
    { solve = Just . sum . fmap (cubePower . cubesNeeded)
    }
  , tests =
    [ unlines
      [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
      , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
      , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
      , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      ] :=> [(PartA, "8"), (PartB, "2286")]
    ]
  }

data Game = Game
  { gameId :: Int
  , gameReveals :: [Reveal]
  }
  deriving (Eq, Ord, Show)

newtype Reveal = Reveal { revealRGB :: V3 Word }
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (V3 (Max Word))

data BallColor = BallR | BallG | BallB
  deriving (Eq, Ord, Show)

gameP :: Parser Game
gameP = do
  "Game "
  i <- decimal
  ": "
  r <- revealP `sepBy1` "; "
  pure Game { gameId = i, gameReveals = r }

revealP :: Parser Reveal
revealP = foldMap Reveal <$> (revealEntryP `sepBy1` ", ")
  where
    revealEntryP = (*^) <$> decimal <*> (space *> colorP)
    colorP = asum
      [ V3 1 0 0 <$ "red"
      , V3 0 1 0 <$ "green"
      , V3 0 0 1 <$ "blue"
      ]

isGamePossible :: Reveal -> Game -> Bool
isGamePossible givens = isRevealUnder givens . cubesNeeded

cubesNeeded :: Game -> Reveal
cubesNeeded = fold . gameReveals

isRevealUnder :: Reveal -> Reveal -> Bool
isRevealUnder (Reveal threshold) (Reveal x) = and (liftA2 (>=) threshold x)

cubePower :: Reveal -> Word
cubePower (Reveal xs) = product xs