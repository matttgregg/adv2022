{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import           Control.Applicative  (Alternative ((<|>)))
import           Data.Either          (rights)
import           Data.List.Index      (indexed)
import           Data.List.Split      (chunksOf)
import qualified Data.Map             as Map
import           Data.Sort            (sort)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Codec.Picture

import           Data.Attoparsec.Text (char, decimal, parseOnly, sepBy, string)

data SpaceFill
  = Rock
  | Sand
  | EmptySpace
  deriving (Show, Eq)

data SpPoint =
  SpPoint Int Int
  deriving (Show, Eq, Ord)

data RockState
  = Falling
  | Escaped
  deriving (Show, Eq)

-- 503,4 -> 502,4 -> 502,9 -> 494,9
pSpPoint = SpPoint <$> decimal <* char ',' <*> decimal

pSegments = pSpPoint `sepBy` string " -> "

pointsAlong (SpPoint x0 y0, SpPoint x1 y1)
  | x0 == x1 && y0 <= y1 = map (x0 `SpPoint`) [y0 .. y1]
pointsAlong (SpPoint x0 y0, SpPoint x1 y1)
  | x0 == x1 = map (x0 `SpPoint`) [y1 .. y0]
pointsAlong (SpPoint x0 y0, SpPoint x1 y1)
  | y0 == y1 && x0 <= x1 = map (`SpPoint` y0) [x0 .. x1]
pointsAlong (SpPoint x0 y0, SpPoint x1 y1)
  | y0 == y1 = map (`SpPoint` y0) [x1 .. x0]

pointsAlongSegments :: [SpPoint] -> [SpPoint]
pointsAlongSegments pts = concatMap pointsAlong $ zip pts (drop 1 pts)

-- The current state of a RockFace
data RockFace =
  RockFace
    { rockMap   :: Map.Map SpPoint SpaceFill
    , minBounds :: SpPoint
    , maxBounds :: SpPoint
    , sand      :: SpPoint -- The currently falling bit of sand.
    , rockState :: RockState
    }
  deriving (Show)

mapAllSegments :: [[SpPoint]] -> Map.Map SpPoint SpaceFill
mapAllSegments segs = foldl (\m pt -> Map.insert pt Rock m) Map.empty pts
  where
    pts = concatMap pointsAlongSegments segs

initRockFace :: [[SpPoint]] -> RockFace
initRockFace segs =
  let initMap = mapAllSegments segs
      pts = Map.keys initMap
      xs = map (\(SpPoint x y) -> x) pts
      ys = 0 : map (\(SpPoint x y) -> y) pts
      minY = min (minimum ys) 0
      maxY = maximum ys
      height = maxY - minY + 5
      minX = min (minimum xs) (500 - height)
      maxX = max (maximum xs) (500 + height)
   in RockFace
        { rockMap = initMap
        , minBounds = SpPoint minX minY
        , maxBounds = SpPoint maxX maxY
        , sand = SpPoint 500 0
        , rockState = Falling
        }

sandEntry = SpPoint 500 0

dropSand :: Bool -> RockFace -> RockFace
dropSand withFloor rf@RockFace { rockMap = rm
                               , minBounds = SpPoint minX minY
                               , maxBounds = SpPoint maxX maxY
                               , sand = oSand@(SpPoint sX sY)
                               } =
  let newDown = SpPoint sX (sY + 1)
      newDownLeft = SpPoint (sX - 1) (sY + 1)
      newDownRight = SpPoint (sX + 1) (sY + 1)
      emptyAboveFloor pt@(SpPoint px py) =
        (not withFloor || py <= maxY + 1) &&
        EmptySpace == Map.findWithDefault EmptySpace pt rm
      (newSand, newState, newRm) =
        if not withFloor && (sY >= maxY || sX < minX || sX > maxX)
          then (oSand, Escaped, rm)
          else if emptyAboveFloor newDown
                 then (newDown, Falling, rm)
                 else if emptyAboveFloor newDownLeft
                        then (newDownLeft, Falling, rm)
                        else if emptyAboveFloor newDownRight
                               then (newDownRight, Falling, rm)
                               else ( sandEntry
                                    -- We terminate if the blocked sand was at the entry
                                    , if sandEntry == oSand
                                        then Escaped
                                        else Falling
                                    , Map.insert oSand Sand rm)
   in rf {rockMap = newRm, sand = newSand, rockState = newState}

printRockFace RockFace { rockMap = rm
                       , minBounds = SpPoint minX minY
                       , maxBounds = SpPoint maxX maxY
                       } =
  map
    (T.pack . (\y -> map (rockCharAt . (`SpPoint` y)) [minX .. maxX]))
    [minY .. maxY]
  where
    rockCharAt pt =
      let rockAt = Map.findWithDefault EmptySpace pt rm
          charFor EmptySpace = ' '
          charFor Rock       = '#'
          charFor Sand       = 'o'
       in charFor rockAt

imageFromRockFace scale RockFace { rockMap = rm
                                 , minBounds = SpPoint minX minY
                                 , maxBounds = SpPoint maxX maxY
                                 , sand = SpPoint sx sy
                                 } =
  let pixelForSpace Sand       = PixelRGB8 214 218 123
      pixelForSpace Rock       = PixelRGB8 120 120 120
      pixelForSpace EmptySpace = PixelRGB8 0 0 0
      pixelForPt x y
        | sx == ((x `div` scale) + minX) && sy == ((y `div` scale) + minY) =
          PixelRGB8 225 225 225
      pixelForPt x y =
        pixelForSpace $
        Map.findWithDefault
          EmptySpace
          (SpPoint ((x `div` scale) + minX) ((y `div` scale) + minY))
          rm
   in generateImage
        pixelForPt
        (scale * (1 + maxX - minX))
        (scale * (4 + maxY - minY))

countSand RockFace {rockMap = rm} = length $ filter (== Sand) $ Map.elems rm

part1 fname = do
  ls <- T.lines <$> TIO.readFile fname
  let segments = rights $ map (parseOnly pSegments) ls
  let rf = initRockFace segments
  let lastSand =
        head $
        dropWhile ((== Falling) . rockState) $ iterate (dropSand False) rf
  --mapM_ TIO.putStrLn $ printRockFace lastSand
  return $ countSand lastSand

part2 fname = do
  ls <- T.lines <$> TIO.readFile fname
  let segments = rights $ map (parseOnly pSegments) ls
  let rf = initRockFace segments
  let lastSand =
        head $ dropWhile ((== Falling) . rockState) $ iterate (dropSand True) rf
  --mapM_ TIO.putStrLn $ printRockFace lastSand
  return $ countSand lastSand

visSand scale fname writeTo withFloor skipSize = do
  ls <- T.lines <$> TIO.readFile fname
  let segments = rights $ map (parseOnly pSegments) ls
  let rf = initRockFace segments
  let pOptions = PaletteOptions MedianMeanCut False 8
  let (sandImage, sandPalette) = palettize pOptions $ imageFromRockFace scale rf
  let framesSand =
        takeWhile ((== Falling) . rockState) $ iterate (dropSand withFloor) rf
  --mapM_ TIO.putStrLn $ printRockFace lastSand
  let imagesSand =
        map (imageFromRockFace scale . head) (chunksOf skipSize framesSand)
  reportRight $ writeGifAnimation writeTo 1 LoopingForever imagesSand
  where
    reportRight (Right x)  = x
    reportRight (Left err) = print err
