{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import           Data.Either          (rights)
import           Data.List            (nub)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Sort            (sort)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Debug.Trace          (trace)

import           Data.Attoparsec.Text (char, decimal, parseOnly, sepBy, string)

data Spt =
  Spt Int Int
  deriving (Show, Eq)

data Sensor =
  Sensor
    { sensorPt :: Spt
    , beaconPt :: Spt
    }
  deriving (Show)

data Interval =
  Interval Int Int
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
  x <- decimal
  string ", y="
  y <- decimal
  string ": closest beacon is at x="
  bx <- decimal
  string ", y="
  buildSensor x y bx <$> decimal

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

coveredBy :: [Interval] -> [Int]
coveredBy intervals =
  let ptsFor (Interval from to) = [from .. to]
      collectInterval m ivl =
        foldl (\m pt -> Map.insert pt True m) m $ ptsFor ivl
      collectedPts = foldl collectInterval Map.empty intervals
   in Map.keys collectedPts

data AccInterval =
  AccInterval
    { lastOpen  :: Int
    , nextClose :: Int
    , counted   :: Int
    }

coveredByCount :: [Interval] -> Int
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

beaconsOn ly sensors =
  let maybeBeacon Sensor {beaconPt = Spt bx by}
        | by == ly = Just bx
      maybeBeacon _ = Nothing
      beacons = mapMaybe maybeBeacon sensors
   in length $ nub beacons

part1 checkLine fname = do
  ls <- T.lines <$> TIO.readFile fname
  let sensors = rights $ map (parseOnly pSensor) ls
  let intervals = mapMaybe (intervalOn checkLine) sensors
  let _ = trace (show intervals) ()
  return $ coveredByCount intervals - beaconsOn checkLine sensors
