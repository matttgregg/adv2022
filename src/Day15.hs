{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import           Data.Either          (rights)
import           Data.List            (nub)
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes, isNothing, mapMaybe)
import           Data.Sort            (sort)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Debug.Trace          (trace)

import           Data.Attoparsec.Text (char, decimal, parseOnly, sepBy, signed,
                                       string)

data Spt =
  Spt Integer Integer
  deriving (Show, Eq)

data Sensor =
  Sensor
    { sensorPt :: Spt
    , beaconPt :: Spt
    }
  deriving (Show)

data Interval =
  Interval Integer Integer
  deriving (Show, Eq)

instance Ord Interval where
  (Interval x1 y1) <= (Interval x2 y2) =
    if x1 == x2
      then y1 <= y2
      else x1 <= x2

buildSensor x y bx by = Sensor {sensorPt = Spt x y, beaconPt = Spt bx by}

-- Sensor at x=2, y=18: closest beacon is at x=-2, y=15
pSensor = do
  string "Sensor at x="
  x <- signed decimal
  string ", y="
  y <- signed decimal
  string ": closest beacon is at x="
  bx <- signed decimal
  string ", y="
  buildSensor x y bx <$> signed decimal

intervalOn y Sensor {sensorPt = Spt sx sy, beaconPt = Spt bx by} =
  let manhattanReach = abs (sx - bx) + abs (sy - by)
      reachOn = (manhattanReach - abs (y - sy))
   in if reachOn < 0
        then Nothing
        else Just $
             {- trace
               ("Interval : " <>
                show (sx - reachOn) <> " -> " <> show (sx + reachOn)) -}
             Interval (sx - reachOn) (sx + reachOn)

coveredBy :: [Interval] -> [Integer]
coveredBy intervals =
  let ptsFor (Interval from to) = [from .. to]
      collectInterval m ivl =
        foldl (\m pt -> Map.insert pt True m) m $ ptsFor ivl
      collectedPts = foldl collectInterval Map.empty intervals
   in Map.keys collectedPts

data AccInterval =
  AccInterval
    { lastOpen  :: Integer
    , nextClose :: Integer
    , counted   :: Integer
    }

coveredByCount :: [Interval] -> Integer
coveredByCount intervals =
  let (Interval fx fy):restSorted = sort intervals
      acc = AccInterval {lastOpen = fx, nextClose = fy, counted = 0}
      accStep a@AccInterval {lastOpen = lo, nextClose = nc, counted = c} (Interval nx ny) =
        if nx <= nc
          then a {nextClose = max nc ny}
          else a {lastOpen = nx, nextClose = ny, counted = c + (nc - lo + 1)}
      AccInterval {lastOpen = lo, nextClose = nc, counted = c} =
        foldl accStep acc restSorted
   in c + (nc - lo + 1)

gapOn :: Integer -> Integer -> [Interval] -> Maybe Integer
gapOn minX maxX intervals =
  let sorted = sort intervals
      acc =
        AccInterval {lastOpen = minX - 1, nextClose = minX - 1, counted = -1}
      accStep a@AccInterval {lastOpen = lo, nextClose = nc, counted = c} (Interval nx ny)
      -- We've found a result, don't do anything more
        | c >= 0 = a
      -- We've gone past the end
        | nc >= maxX = a {counted = maxX + 1}
      -- We have an overlap or adjacent
        | nx <= (nc + 1) = a {nextClose = max nc ny}
      -- We have a gap!
        | otherwise = a {counted = nc + 1}
      AccInterval {counted = c} = foldl accStep acc sorted
   in if c >= minX && c <= maxX
        then Just c
        else Nothing

beaconsOn ly sensors =
  let maybeBeacon Sensor {beaconPt = Spt bx by}
        | by == ly = Just bx
      maybeBeacon _ = Nothing
      beacons = mapMaybe maybeBeacon sensors
   in fromIntegral $ length $ nub beacons

part1 checkLine fname = do
  ls <- T.lines <$> TIO.readFile fname
  let sensors = rights $ map (parseOnly pSensor) ls
  let intervals = mapMaybe (intervalOn checkLine) sensors
  let _ = trace (show intervals) ()
  return $ coveredByCount intervals - beaconsOn checkLine sensors

part2 maxSize fname = do
  ls <- T.lines <$> TIO.readFile fname
  let sensors = rights $ map (parseOnly pSensor) ls
  let intervalsOn row = mapMaybe (intervalOn row) sensors
  let allIntervalsOn row = map (intervalOn row) sensors
  {- mapM_
    (\i -> print ("On: " <> show i <> ":" <> show (allIntervalsOn i)))
    [0 .. 20] -- MTG Smaller for debug
    -}
  let lineGaps = map (gapOn 0 maxSize . intervalsOn) [0 .. maxSize]
  --_ <- trace (show $ catMaybes lineGaps) $ return ()
  let gapY = fromIntegral $ length $ takeWhile isNothing lineGaps
  let Just gapX = head $ dropWhile isNothing lineGaps
  --_ <- trace ("Found a gap at " <> show gapX <> "," <> show gapY) $ return ()
  --print $ "Sensors : " <> show (head sensors)
  return $ gapX * 4000000 + gapY
