{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import           Data.Array       (elems)
import           Data.List.Index  (indexed)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Data.Void

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST

-- Find the crates on a line of input
cratesOnLine l = map (T.index l) [1,5 .. (T.length l - 1)]

moveCrates crateLines moves =
  runSTArray $ do
    let end = length (head crateLines)
    crateArray <- newArray (1, end) []
    forM_ crateLines $ \l ->
      forM_ (indexed l) $ \(i, ch) -> do
        oldVal <- readArray crateArray (i + 1)
        when (ch /= ' ') $ writeArray crateArray (i + 1) (ch : oldVal)
    -- Now do the moves, which are (from, to, count) triples
    forM_ moves $ \(from, to, count) ->
      forM_ [1 .. count] $ \_ -> do
        oldFrom <- readArray crateArray from
        oldTo <- readArray crateArray to
        writeArray crateArray from (tail oldFrom)
        writeArray crateArray to (head oldFrom : oldTo)
    return crateArray

moveCratesOrdered crateLines moves =
  runSTArray $ do
    let end = length (head crateLines)
    crateArray <- newArray (1, end) []
    forM_ crateLines $ \l ->
      forM_ (indexed l) $ \(i, ch) -> do
        oldVal <- readArray crateArray (i + 1)
        when (ch /= ' ') $ writeArray crateArray (i + 1) (ch : oldVal)
    -- Now do the moves, which are (from, to, count) triples
    forM_ moves $ \(from, to, count) -> do
      oldFrom <- readArray crateArray from
      oldTo <- readArray crateArray to
      let moving = take count oldFrom
      writeArray crateArray from (drop count oldFrom)
      writeArray crateArray to (moving <> oldTo)
    return crateArray

isMoveLine l =
  let ws = T.words l
   in not (null ws) && head ws == "move"

asMoves :: T.Text -> (Int, Int, Int)
asMoves l =
  let ws = T.words l
   in (indexedInt ws 3, indexedInt ws 5, indexedInt ws 1)
  where
    indexedInt ws ix = read $ T.unpack $ ws !! ix :: Int

part1 fName = do
  fileContent <- TIO.readFile fName
  let fileLines = T.lines fileContent
  let crateLines = reverse $ takeWhile (T.elem '[') fileLines
  let crates = map cratesOnLine crateLines
  let moveLines = filter isMoveLine fileLines
  let moves = map asMoves moveLines
  let crateState = moveCrates crates moves
  let topCrates = map head $ elems crateState
  return topCrates

part2 fName = do
  fileContent <- TIO.readFile fName
  let fileLines = T.lines fileContent
  let crateLines = reverse $ takeWhile (T.elem '[') fileLines
  let crates = map cratesOnLine crateLines
  let moveLines = filter isMoveLine fileLines
  let moves = map asMoves moveLines
  let crateState = moveCratesOrdered crates moves
  let topCrates = map head $ elems crateState
  return topCrates
