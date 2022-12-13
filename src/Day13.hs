{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import           Control.Applicative  (Alternative ((<|>)))
import           Data.Either          (rights)
import           Data.List.Index      (indexed)
import           Data.List.Split      (chunksOf)
import           Data.Sort            (sort)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Data.Attoparsec.Text

data Nested
  = ListOf [Nested]
  | NInt Int
  deriving (Show, Eq)

pNested =
  (NInt <$> decimal) <|>
  (ListOf <$> (char '[' *> pNested `sepBy` char ',' <* char ']'))

nCompare :: Nested -> Nested -> Int
nCompare (NInt l) (NInt r)
  | l < r = 1
nCompare (NInt l) (NInt r)
  | l > r = -1
nCompare (NInt l) (NInt r) = 0
nCompare (NInt l) rL = nCompare (ListOf [NInt l]) rL
nCompare lL (NInt r) = nCompare lL (ListOf [NInt r])
nCompare (ListOf []) (ListOf []) = 0
nCompare (ListOf []) _ = 1 -- Left runs out first, is in order.
nCompare _ (ListOf []) = -1 -- Right runs out first, is out of order.
nCompare (ListOf (l:ls)) (ListOf (r:rs)) =
  let elCheck = nCompare l r
   in if elCheck /= 0
        then elCheck
        else nCompare (ListOf ls) (ListOf rs)

instance Ord Nested where
  l <= r = nCompare l r >= 0

nOrdered [l, r] = nCompare l r > 0

Right divider1 = parseOnly pNested "[[2]]"

Right divider2 = parseOnly pNested "[[6]]"

part1 fname = do
  lines <- T.lines <$> TIO.readFile fname
  let parsed = chunksOf 2 $ rights $ map (parseOnly pNested) lines
  let matched = map ((1 +) . fst) $ filter snd $ indexed $ map nOrdered parsed
  return $ sum matched

part2 fname = do
  lines <- T.lines <$> TIO.readFile fname
  let parsed = rights $ map (parseOnly pNested) lines
  let sortedParsed = sort $ divider1 : divider2 : parsed
  -- We want to find the locations of the two dividers
  let matched =
        map ((1 +) . fst) $
        filter (\(_, nst) -> nst `elem` [divider1, divider2]) $
        indexed sortedParsed
  return $ product matched
