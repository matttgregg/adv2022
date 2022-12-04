{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.Text            as T (Text)
import qualified Data.Text.IO         as TIO (readFile)
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void T.Text

digits :: Parser Int
digits = read <$> some digitChar

pRange :: Parser (Int, Int)
pRange = do
  from <- digits
  _ <- char '-'
  to <- digits
  return (from, to)

pRanges :: Parser [(Int, Int)]
pRanges = do
  rangeOne <- pRange
  _ <- char ','
  rangeTwo <- pRange
  return [rangeOne, rangeTwo]

line = do
  rs <- pRanges
  optional newline
  return rs

fileRanges = many line

takeRight (Left _)  = undefined
takeRight (Right x) = x

containedPair [(from1, to1), (from2, to2)] =
  (from1 >= from2 && to1 <= to2) || (from2 >= from1 && to2 <= to1)

containingPairs = map containedPair

intersectingPair [(from1, to1), (from2, to2)] =
  (from1 >= from2 && from1 <= to2) || (from2 >= from1 && from2 <= to1)

part1 fName =
  length . filter id <$> containingPairs <$> takeRight <$>
  parse fileRanges fName <$>
  TIO.readFile fName

part2 fName =
  length . filter id <$> map intersectingPair <$> takeRight <$>
  parse fileRanges fName <$>
  TIO.readFile fName
