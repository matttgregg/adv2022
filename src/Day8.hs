{-# LANGUAGE TupleSections #-}

module Day8 where

import           Data.List       (find, nub, splitAt)
import           Data.List.Index (indexed)
import           Data.Maybe      (isJust)

newtype Forest =
  Forest [[Int]]
  deriving (Show)

forestFrom :: String -> Forest
forestFrom s =
  let rows = lines s
   in Forest $ map doRow rows
  where
    doRow = map (\c -> read [c] :: Int)

visibleAlong :: [Int] -> [Int]
visibleAlong l = fst $ foldl canSee ([], -1) $ indexed l
  where
    canSee (seen, tmax) (i, t) =
      if t > tmax
        then (i : seen, t)
        else (seen, tmax)

visibleInForest :: Forest -> [(Int, Int)]
visibleInForest (Forest f) =
  let width = length $ head f
      height = length f
      seenLeft = map visibleAlong $ f
      coordsLeft = concatMap (\(i, c) -> map (, i) c) $ indexed seenLeft
      seenRight = map (visibleAlong . reverse) f
      coordsRight =
        concatMap (\(i, cs) -> map (\ci -> (width - ci - 1, i)) cs) $
        indexed seenRight
      fromTop = map (\i -> map (!! i) f) [0 .. width - 1]
      seenTop = map visibleAlong fromTop
      coordsTop = concatMap (\(i, c) -> map (i, ) c) $ indexed seenTop
      seenBottom = map (visibleAlong . reverse) fromTop
      coordsBottom =
        concatMap (\(i, cs) -> map (\ci -> (i, height - ci - 1)) cs) $
        indexed seenBottom
   in nub $ concat [coordsLeft, coordsRight, coordsTop, coordsBottom]

viewsFor :: Int -> Int -> Forest -> [[Int]]
viewsFor row col (Forest f) =
  let rowView = f !! row
      (lView', rView') = splitAt col rowView
      lView = reverse lView'
      rView =
        if null rView'
          then []
          else tail rView'
      colView = map (!! col) f
      (upView', downView') = splitAt row colView
      upView = reverse upView'
      downView =
        if null downView'
          then []
          else tail downView'
   in [lView, rView, upView, downView]

scoreView :: Int -> [Int] -> Int
scoreView i cs = length (takeWhile (< i) cs) + adjust
  where
    adjust =
      if isJust (find (>= i) cs)
        then 1
        else 0

scoreFor :: (Int, Int) -> Forest -> Int
scoreFor (row, col) (Forest f) =
  let views = viewsFor row col (Forest f)
      val = f !! row !! col
   in product (map (scoreView val) views)

maxScore (Forest f) =
  let width = length $ head f
      height = length f
      candidates = [(r, c) | r <- [0 .. height - 1], c <- [0 .. width - 1]]
   in maximum (map (`scoreFor` Forest f) candidates)

part1 f = length . visibleInForest . forestFrom <$> readFile f

part2 f = maxScore . forestFrom <$> readFile f
