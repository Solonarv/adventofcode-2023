{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import AOC.Solution
import ParsingPrelude
import Util

import Data.Finite
import Data.List (sortOn, sortBy)
import Data.List.NonEmpty (groupAllWith)
import Data.Function (on)

solution :: Solution [([Card], Int)] Int Int
solution = Solution
  { decodeInput = (`sepBy` eol) do
      hand <- replicateM 5 cardP
      hspace1
      bid <- decimal
      pure (hand, bid)
  , solveA = defSolver
  { solve = Just . aggregateWinnings cmpHands
  }
  , solveB = defSolver
  { solve = Just . aggregateWinnings cmpHandsJokingly
  }
  , tests =
    [ unlines
      [ "32T3K 765"
      , "T55J5 684"
      , "KK677 28"
      , "KTJJT 220"
      , "QQQJA 483"
      ] :=> [(PartA, "6440"), (PartB, "5905")]
    ]
  }

data Card = N (Finite 10) | T | J | Q | K | A
  deriving (Eq, Ord, Show)

cardP :: Parser Card
cardP = asum
  [ A <$ "A"
  , K <$ "K"
  , Q <$ "Q"
  , J <$ "J"
  , T <$ "T"
  , N <$> (singleDigit 10 >>= guarding (>1))
  ]

aggregateWinnings :: ([Card] -> [Card] -> Ordering) -> [([Card], Int)] -> Int
aggregateWinnings cmpH = sum' . fmap (\(rank, (_, bid)) -> rank * bid) . zip [1..] . sortBy (cmpH `on` fst)

data HandStrength = HighCard | OnePair | TwoPair | ThreeOf | FullHouse | FourOf | FiveOf
  deriving (Eq, Ord, Show)

classifyHand :: [Card] -> HandStrength
classifyHand xs = case sortOn length $ groupAllWith id xs of
  [] -> error "classifyHand: empty hand"
  [_] -> FiveOf
  [_, bs]
    | length bs == 4
    -> FourOf
    | length bs == 3
    -> FullHouse
  [_, bs, cs]
    | length cs == 3
    -> ThreeOf
    | length bs == 2
    -> TwoPair
  [_,_,_,_] -> OnePair
  _ -> HighCard

cmpHands :: [Card] -> [Card] -> Ordering
cmpHands = (compare `on` classifyHand) <> compare

cmpHandsJokingly :: [Card] -> [Card] -> Ordering
cmpHandsJokingly = (compare `on` classifyHandJokingly) <> (compare `on` fmap DevalueJ)

newtype DevalueJ = DevalueJ Card
  deriving newtype (Show, Eq)

instance Ord DevalueJ where
  DevalueJ J <= _ = True
  DevalueJ x <= DevalueJ J = x == J
  DevalueJ x <= DevalueJ y = x <= y

classifyHandJokingly :: [Card] -> HandStrength
classifyHandJokingly = maximum . fmap classifyHand . subJokers

-- all possible hands that can be obtained by substituting jokers
subJokers :: [Card] -> [[Card]]
subJokers = traverse \case
  J -> fmap N [2..9] ++ [T, Q, K, A]
  x -> [x]