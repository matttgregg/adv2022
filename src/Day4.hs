{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.HashMap.Strict  as H (HashMap, empty, findWithDefault,
                                            fromListWith, insert, keys)
import           Data.List            (intersect, nub, union, (\\))
import           Data.List.Index      (indexed)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO (readFile)
import           Data.Void

import           Debug.Trace

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

makeRanges rangePairs = (firstMap, lastMap)
  where
    firstMap = H.fromListWith mappend (map flipPair allFirst)
    lastMap = H.fromListWith mappend (map flipPair allLast)
    allFirst = indexed $ map fst $ concat rangePairs
    allLast = indexed $ map snd $ concat rangePairs
    flipPair (a, b) = (b, [a])

extrema (firstMap, lastMap) = (lowest, highest)
  where
    lowest = minimum (H.keys firstMap <> H.keys lastMap)
    highest = maximum (H.keys firstMap <> H.keys lastMap)

data OpenAt =
  OpenAt
    { open        :: H.HashMap Int [Int]
    , runningOpen :: [Int]
    , closingNext :: [Int]
    }
  deriving (Show)

openMap ranges@(openRanges, closeRanges) --(rFrom, rTo, [rFrom .. rTo])
 =
  let m = foldl collectAt initial [rFrom .. rTo]
   in open m
  where
    (rFrom, rTo) = extrema ranges
    initial = OpenAt {open = H.empty, runningOpen = [], closingNext = []}
    {-
      We need to do multiple things:
        Those opening are added to the runningOpen.
        The open map is taken from the new runningOpen.
    -}
    collectAt openAt index =
      OpenAt
        { open = newOpen
        , runningOpen = newRunningOpen
        , closingNext = newClosingNext
        }
        -- The new running open the existing running, plus opening, minus closing
      where
        opening = H.findWithDefault [] index openRanges
        newClosingNext = H.findWithDefault [] index closeRanges
        newRunningOpen =
          (union opening (runningOpen openAt)) \\ (closingNext openAt)
        newOpen = H.insert index newRunningOpen (open openAt)

takeRight (Left _)  = undefined
takeRight (Right x) = x

intersects oMap (index, (from, to)) =
  trace
    ("\n" <>
     (show from) <>
     "-" <> (show to) <> ":" <> (show atOpen) <> "::" <> (show atClose))
    isContained
  where
    nonEmpty x = (length x) > 0
    atOpen = H.findWithDefault [] from oMap
    atClose = H.findWithDefault [] to oMap
    isContained = (intersect atOpen atClose) \\ [index]

findIntersections pairedRanges = map (intersects oMap) indexedRanges
  where
    oMap = (openMap . makeRanges) pairedRanges
    indexedRanges = indexed $ concat pairedRanges

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
