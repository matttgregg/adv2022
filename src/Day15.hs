{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import           Data.Either          (rights)
import           Data.List            (nub)
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes, fromJust, isNothing, mapMaybe)
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
        | nx <= nc + 1 = a {nextClose = max nc ny}
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

slowPart2 maxSize fname = do
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
  --print ("Found a gap at " <> show gapX <> "," <> show gapY)
  --print $ "Sensors : " <> show (head sensors)
  return $ gapX * 4000000 + gapY

-- A Line Segment with fomula y = (+/-)x + dx, x in [minX, maxX]
data LineSeg =
  LineSeg
    { xRange   :: Interval
    , dx       :: Integer
    , positive :: Bool
    }
  deriving (Show, Eq)

--   0 1 2 3 4 5 6
-- 0 . . . . . . .
-- 1 . . . . . . .
-- 2 . . . o . . .
-- 3 . . o # o . .
-- 4 . o B S # o .
-- 5 . . o # o . .
-- 6 . . . o . . .
-- 7 . . . . . . .
--
-- m = 1
-- sx = 3, sy = 4
-- NE = [3, 5], -1  + x
-- NW = [1, 3], 5 - x
-- SE = [3, 5], 9 - x
-- SW = [1, 3], 3 + x
-- For +ve seg : y = dx + x => dx = y - x, y = y0 + m
-- For -ve seg : y = dx - x => dx = y + x
-- y = dx1 + x
-- y = dx2 + x
segmentsForSensor Sensor {sensorPt = Spt sx sy, beaconPt = Spt bx by} =
  let m = abs (sx - bx) + abs (sy - by)
      neSeg =
        LineSeg
          { xRange = Interval sx (sx + m + 1)
          , dx = (sy - m - 1) - sx
          , positive = True
          }
      nwSeg =
        LineSeg
          { xRange = Interval (sx - m - 1) sx
          , dx = (sy - m - 1) + sx
          , positive = False
          }
      seSeg =
        LineSeg
          { xRange = Interval sx (sx + m + 1)
          , dx = (sy + m + 1) + sx
          , positive = False
          }
      swSeg =
        LineSeg
          { xRange = Interval (sx - m - 1) sx
          , dx = (sy + m + 1) - sx
          , positive = True
          }
   in [neSeg, nwSeg, seSeg, swSeg]

intervalOverlap (Interval x0 y0) (Interval x1 y1)
  | x1 < x0 = intervalOverlap (Interval x1 y1) (Interval x0 y0)
  | x1 <= y0 = Just $ Interval x1 (min y0 y1)
  | otherwise = Nothing

intervalStart (Interval x0 _) = x0

segPoint x LineSeg {xRange = xr0, dx = dx0, positive = p0}
  | p0 = Spt x (x + dx0)
  | otherwise = Spt x (-x + dx0)

ySegRange l@LineSeg {xRange = Interval xFrom xTo, dx = dx0, positive = p0} =
  let y0 = (yFromPt $ segPoint xFrom l)
      y1 = (yFromPt $ segPoint xTo l)
   in if y0 <= y1
        then Interval (max y0 0) (min y1 4000000)
        else Interval (max y1 0) (min y0 4000000)

segmentOverlap l0@LineSeg {xRange = xr0, dx = dx0, positive = p0} l1@LineSeg { xRange = xr1
                                                                             , dx = dx1
                                                                             , positive = p1
                                                                             }
  | p0 /= p1 = Nothing
  | l0 == l1 = Nothing
  | otherwise =
    let xOverlap = intervalOverlap xr0 xr1
     in (if isNothing xOverlap || dx0 /= dx1
           then Nothing
           else Just l0 {xRange = fromJust xOverlap})

isInInterval x (Interval x0 y0) = x >= x0 && x <= y0 || x >= y0 && x <= x0

segmentCross l0@LineSeg {xRange = xr0, dx = dx0, positive = p0} l1@LineSeg { xRange = xr1
                                                                           , dx = dx1
                                                                           , positive = p1
                                                                           }
  | p0 == p1 = Nothing
  | p1 = segmentCross l1 l0 -- Ensure p0 = true, p1 = false
  | otherwise =
    let doubleX = dx1 - dx0
        x = doubleX `div` 2
     in if odd doubleX
          then Nothing
          else if x `isInInterval` xr0 && x `isInInterval` xr1
                 then Just $ Spt x (x + dx0)
                 else Nothing

-- y = x + x0, y = -x + x1 => 2x = x1 - x0
yFromPt (Spt _ y) = y

nudgesSensor (Spt x y) s@Sensor {sensorPt = Spt sx sy, beaconPt = Spt bx by} =
  let m = abs (sx - bx) + abs (sy - by)
      mToS = abs (sx - x) + abs (sy - y)
   in if mToS - 1 == m
        then Just s
        else Nothing

printSeg l0@LineSeg {xRange = Interval xFrom xTo, dx = dx0, positive = p0} =
  if p0
    then "[" <> show xFrom <> ", " <> show xTo <> "] := x + " <> show dx0
    else "[" <> show xFrom <> ", " <> show xTo <> "] := -x + " <> show dx0

debugPart2 fname = do
  ls <- T.lines <$> TIO.readFile fname
  let sensors = rights $ map (parseOnly pSensor) ls
  let toCheck = Spt 2706598 3253551
  print $ "Checking " <> show toCheck
  let nudging = mapMaybe (nudgesSensor toCheck) sensors
  print "All Edges::"
  print nudging
  let nudgingEdges = concatMap segmentsForSensor nudging
  mapM_
    (\l -> print $ printSeg l <> " -> " <> show (segPoint 2706598 l))
    nudgingEdges
  let overlaps =
        catMaybes
          [segmentOverlap s0 s1 | s0 <- nudgingEdges, s1 <- nudgingEdges]
  print "Overlapping::"
  mapM_ (print . printSeg) overlaps
  let crossings =
        catMaybes [segmentCross s0 s1 | s0 <- overlaps, s1 <- overlaps]
  return crossings

part2 maxSize fname = do
  ls <- T.lines <$> TIO.readFile fname
  let sensors = rights $ map (parseOnly pSensor) ls
  let allSegments = concatMap segmentsForSensor sensors
  let sharedSegments =
        catMaybes [segmentOverlap s0 s1 | s0 <- allSegments, s1 <- allSegments]
  --print $ sort $ map ySegRange sharedSegments
  let overlaps =
        catMaybes
          [segmentCross s0 s1 | s0 <- sharedSegments, s1 <- sharedSegments]
  let inRange (Spt x y) = x >= 0 && x <= maxSize && y >= 0 && y <= maxSize
  let allowed = filter inRange overlaps
  --print allowed
  let candidateYs = nub $ map yFromPt allowed
  --print $ sort candidateYs
  let intervalsOn row = mapMaybe (intervalOn row) sensors
  let tryRow y =
        let res = gapOn 0 maxSize (intervalsOn y)
         in if isNothing res
              then Nothing
              else Just (y, fromJust res)
  let lineGaps = mapMaybe tryRow candidateYs
  --print $ sort lineGaps
  let gapY = fst $ head lineGaps
  let gapX = snd $ head lineGaps
  -- Looking for: "Found a gap at 2706598,3253551"
  --_ <- trace ("Found a gap at " <> show gapX <> "," <> show gapY) $ return ()
  return $ gapX * 4000000 + gapY
