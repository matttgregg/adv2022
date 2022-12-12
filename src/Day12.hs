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

data NbrMode
  = NbrsUp
  | NbrsDown
  deriving (Eq, Show)

neighbours nbrMode o@(Pt x y) ForestMap {heights = hs} =
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
       in if nbrMode == NbrsUp
            then cHeight - nbrHeight >= -1
            else nbrHeight - cHeight >= -1

data PathMap =
  PathMap
    { forest     :: ForestMap
    , distances  :: Map.Map Pt Int
    , working    :: [Pt]
    , newWorking :: [Pt]
    , dist       :: Int
    , nbrMode    :: NbrMode
    , terminated :: Bool
    }
  deriving (Show)

initPathMap nr fm@ForestMap {startPt = s, endPt = e} =
  let distances = Map.insert s 0 Map.empty
   in PathMap
        { forest = fm
        , distances = distances
        , working =
            if nr == NbrsUp
              then [s]
              else [e]
        , newWorking = []
        , dist = 0
        , nbrMode = nr
        , terminated = False
        }

-- Do a single step from a pt w
doStepFrom pm@PathMap { forest = f@ForestMap {heights = hs, endPt = ep}
                      , distances = ds
                      , dist = d
                      , newWorking = nw
                      , nbrMode = nr
                      , terminated = oldTerminated
                      } w =
  let nbrs = neighbours nr w f
      newPts = filter (`Map.notMember` ds) nbrs -- These are pts that we haven't already visited
      newHeights = map (\p -> Map.findWithDefault 1 p hs) newPts
      newDistances = foldl (addDistance $ d + 1) ds newPts
      addDistance dist distances newPt =
        Map.insertWith (\_new old -> old) newPt dist distances
      newTerminated =
        if nr == NbrsUp
          then ep `elem` newPts
          else 0 `elem` newHeights
   in pm
        { distances = newDistances
        , newWorking = nw <> newPts
        , terminated = oldTerminated || newTerminated
        }

doStep pm@PathMap {working = ws} =
  let newPm = foldl doStepFrom pm ws
   in newPm {working = newWorking newPm, newWorking = [], dist = 1 + dist newPm}

fastestPath nrm forest =
  let pmap = initPathMap nrm forest
      paths = iterate doStep pmap
   in length $ takeWhile (not . terminated) paths

part1 fname = do
  file <- readFile fname
  let forest = readHeights $ lines file
  return $ fastestPath NbrsUp forest

part2 fname = do
  file <- readFile fname
  let forest = readHeights $ lines file
  return $ fastestPath NbrsDown forest
