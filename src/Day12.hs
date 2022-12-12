module Day12 where

import           Data.Char       (ord)
import           Data.List       (sort)
import           Data.List.Index (indexed)
import qualified Data.Map        as Map
import           Debug.Trace     (trace)

data Pt =
  Pt Int Int
  deriving (Show, Eq, Ord)

data ForestMap =
  ForestMap
    { heights :: Map.Map Pt Int
    , startPt :: Pt
    , endPt   :: Pt
    }
  deriving (Show)

readHeights :: [String] -> ForestMap
readHeights ls =
  let emptyForest =
        ForestMap {heights = Map.empty, startPt = Pt 0 0, endPt = Pt 0 0}
   in foldl readHeightLine emptyForest $ indexed ls
  where
    readHeightLine forest (row, cs) = foldl (readHeight row) forest $ indexed cs
    readHeight row (ForestMap { heights = oheights
                              , startPt = ostart
                              , endPt = oend
                              }) (col, c) =
      let height' c
            | c == 'S' = 0
          height' c
            | c == 'E' = 25
          height' c = ord c - ord 'a'
          newStart =
            if c == 'S'
              then Pt col row
              else ostart
          newEnd =
            if c == 'E'
              then Pt col row
              else oend
          newHeights = Map.insert (Pt col row) (height' c) oheights
       in ForestMap {heights = newHeights, startPt = newStart, endPt = newEnd}

neighbours ar o@(Pt x y) ForestMap {heights = hs} =
  let maybeNbrs = [Pt (x + dx) (y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
   in filter canMove maybeNbrs
  where
    cHeight = Map.findWithDefault 0 o hs
    canMove (Pt x' y')
      | x' == x && y' == y = False
    canMove (Pt x' y')
      | x' /= x && y' /= y = False -- Disallow diagonal moves.
    canMove nbr
      | Map.notMember nbr hs = False
    canMove nbr =
      let nbrHeight = Map.findWithDefault 0 nbr hs
       in cHeight - nbrHeight >= -1 && (ar || nbrHeight > 0)

data PathMap =
  PathMap
    { forest      :: ForestMap
    , distances   :: Map.Map Pt Int
    , working     :: [Pt]
    , newWorking  :: [Pt]
    , dist        :: Int
    , allowReturn :: Bool
    }
  deriving (Show)

initPathMap ar fm@ForestMap {startPt = s} =
  let distances = Map.insert s 0 Map.empty
   in PathMap
        { forest = fm
        , distances = distances
        , working = [s]
        , newWorking = []
        , dist = 0
        , allowReturn = ar
        }

-- Do a single step from a pt w
doStepFrom pm@PathMap { forest = f
                      , distances = ds
                      , dist = d
                      , newWorking = nw
                      , allowReturn = ar
                      } w =
  let nbrs = neighbours ar w f
      newPts = filter (`Map.notMember` ds) nbrs -- These are pts that we haven't already visited
      newDistances = foldl (addDistance $ d + 1) ds newPts
      addDistance dist distances newPt =
        Map.insertWith (\_new old -> old) newPt dist distances
   in pm {distances = newDistances, newWorking = nw <> newPts}

doStep pm@PathMap {working = ws} =
  let newPm = foldl doStepFrom pm ws
   in newPm {working = newWorking newPm, newWorking = [], dist = 1 + dist newPm}

distToEnd :: PathMap -> Int
distToEnd PathMap {forest = ForestMap {endPt = ep}, distances = ds} =
  Map.findWithDefault (-1) ep ds

-- Note that we also terminate if we run out of moves.
foundEnd pm = distToEnd pm >= 0 || null (working pm)

fastestPath ar forest =
  let pmap = initPathMap ar forest
      paths = iterate doStep pmap
      solved = dropWhile (not . foundEnd) paths
   in distToEnd $ head solved

startingPts ForestMap {heights = hs} =
  filter (\pt -> 0 == Map.findWithDefault 0 pt hs) $ Map.keys hs

startingForests fm =
  let starts = startingPts fm
   in map (\s -> fm {startPt = s}) starts

part1 fname = do
  file <- readFile fname
  let forest = readHeights $ lines file
  return $ fastestPath True forest

part2 fname = do
  file <- readFile fname
  let forest = readHeights $ lines file
  let trials = startingForests forest
  let _ = trace ("Checking " <> show (length trials) <> " starting points.") ()
  return $ minimum (filter (> 0) $ map (fastestPath False) trials)
