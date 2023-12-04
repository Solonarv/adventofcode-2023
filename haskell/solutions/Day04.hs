{-# LANGUAGE OverloadedStrings #-}
module Day04 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Bifunctor (first)
import Data.Set (Set)
import qualified Data.Set as Set

solution :: Solution [ScratchCard] Word Word
solution = Solution
  { decodeInput = scratchCardP `sepBy1` eol
  , solveA = defSolver
  { solve = Just . sum' . fmap (expValue . cardValue)
  }
  , solveB = defSolver
  { solve = Just . foldCardValues . fmap ((1,) . cardValue)
  }
  , tests =
    [  unlines
      [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
      , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
      , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
      , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
      , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
      ] :=> [(PartA, "13"), (PartB, "30")]
    ]
  }

data ScratchCard = ScratchCard
  { cardId :: Word
  , cardNumbers :: Set Word
  , cardWinners :: Set Word
  }

scratchCardP :: Parser ScratchCard
scratchCardP = do
  "Card"; hspace
  n <- decimal
  ":"; hspace
  winners <- Set.fromList <$> (decimal `endBy` space1)
  "|"; hspace
  haves <- Set.fromList <$> (decimal `sepBy` hspace1)
  pure ScratchCard { cardId = n, cardNumbers = haves, cardWinners = winners}

cardValue :: ScratchCard -> Word
cardValue card = fromIntegral $ Set.size $ cardNumbers card `Set.intersection` cardWinners card

expValue :: Word -> Word
expValue 0 = 0
expValue n = 2 ^ (n-1)

foldCardValues :: [(Word, Word)] -> Word
foldCardValues [] = 0
foldCardValues ((n,c):cs) = n + foldCardValues (inc c n cs)

inc :: Word -> Word -> [(Word, b)] -> [(Word, b)]
inc c n xs = let
  (front, back) = splitAt (fromIntegral c) xs
  in fmap (first (n+)) front ++ back