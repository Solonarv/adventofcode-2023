{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
module Day03 where

import AOC.Solution
import ParsingPrelude
import Util
import Grid2D

import Data.Char (isDigit, isSpace)
import Data.List.NonEmpty (groupBy, NonEmpty(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Lens
import Linear.V2

solution :: Solution (Grid2D Entry) Word ()
solution = Solution
  { decodeInput = fromLines <$> inputLineP `sepBy1` eol
  , solveA = defSolver
    { solve = Just . getSum . foldMap (Sum . snd) . findPartNumbers
    }
  , solveB = defSolver
  , tests =
    [ unlines
      [ "467..114.."
      , "...*......"
      , "..35..633."
      , "......#..."
      , "617*......"
      , ".....+.58."
      , "..592....."
      , "......755."
      , "...$.*...."
      , ".664.598.."
      ] :=> [(PartA, "4361")]
    ]
  }

data Entry = PartNumber Word Int | PartId Char | Blank
  deriving (Eq, Ord, Show)

isPartId :: Entry -> Bool
isPartId (PartId _) = True
isPartId _ = False

entry2char :: Entry -> Char
entry2char (PartNumber n o) = show n !! o
entry2char (PartId c) = c
entry2char Blank = '.'

inputLineP :: Parser [Entry]
inputLineP = combineNumbers <$> takeWhile1P (Just "non-space character") (not . isSpace)

combineNumbers :: String -> [Entry]
combineNumbers = concatMap transformGroup . groupDigits
  where
    groupDigits = groupBy confuseDigits
    confuseDigits a b = a == b || (isDigit a && isDigit b)
    transformGroup (c :| cs)
      | isDigit c, s <- c:cs, n <- read s = [PartNumber n o | (o,_) <- zip [0..] s]
      | c == '.' = Blank <$ (c:cs)
      | otherwise = PartId <$> (c:cs)

findPartNumbers :: Grid2D Entry -> Set (V2 Int, Word)
findPartNumbers g = Set.fromList
    [ (V2 (x-o) y, n)
    | x <- [0 .. width g-1]
    , y <- [0 .. height g-1]
    , Just (PartNumber n o) <- [g ^? gridPoint x y]
    , any isPartId (adjacents x y g)
    ]
