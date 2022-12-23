{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import           Data.Either          (rights)
import           Data.List            (nub, (\\))
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes, fromJust, isNothing, mapMaybe)
import           Data.Sort            (sort)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Debug.Trace          (trace)

import           Data.Attoparsec.Text (char, decimal, eitherP, letter, many1,
                                       parseOnly, sepBy, signed, string)

data Valve =
  Valve
    { valveName :: T.Text
    , paths     :: [T.Text]
    , flowRate  :: Int
    }
  deriving (Show)

noValve = Valve {valveName = "", paths = [], flowRate = 0}

pVName = T.pack <$> many1 letter

-- Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
-- Valve HH has flow rate=22; tunnel leads to valve GG
pValve = do
  string "Valve "
  vName <- pVName
  string " has flow rate="
  fRate <- decimal
  string "; "
  eitherP (string "tunnels lead to valves ") (string "tunnel leads to valve ")
  pathsTo <- pVName `sepBy` string ", "
  return Valve {valveName = vName, paths = pathsTo, flowRate = fRate}

data RiverState =
  RiverState
    { atValve     :: T.Text
    , timeLeft    :: Int
    , stillClosed :: Map.Map T.Text Int
    , cFlow       :: Int
    , journeyMap  :: Map.Map T.Text (Map.Map T.Text Int)
    }
  deriving (Show)

newtype FastestRoutes =
  FastestRoute (Map.Map (T.Text, T.Text) Int)

data JourneyState =
  JourneyState
    { valves          :: Map.Map T.Text Valve -- Our master valve map.
    , currentDistance :: Int -- The current distance travelled.
    , currentValves   :: [T.Text] -- The valves that we're currently stepping from.
    , bestJourneys    :: Map.Map T.Text Int -- The best journeys found.
    }

journeyDone JourneyState { valves = valves
                         , bestJourneys = bestJourneys
                         , currentValves = cvs
                         } =
  null cvs || length (Map.keys valves) == length (Map.keys bestJourneys)

journeyFrom :: JourneyState -> JourneyState
journeyFrom js@JourneyState { valves = valves
                            , currentDistance = cd
                            , currentValves = cValves
                            , bestJourneys = bestJs
                            } =
  let moves =
        concatMap (\v -> paths $ Map.findWithDefault noValve v valves) cValves
      newMoves = nub moves \\ Map.keys bestJs
      addingBest = Map.fromList (zip newMoves (repeat $ cd + 1))
      newBest = Map.union bestJs addingBest
   in js
        { currentDistance = cd + 1
        , currentValves = newMoves
        , bestJourneys = newBest
        }

findFastest valves =
  let allValves = Map.keys valves
      startState v =
        JourneyState
          { valves = valves
          , currentDistance = 0
          , currentValves = [v]
          , bestJourneys = Map.fromList [(v, 0)]
          }
      findFastestOne v =
        bestJourneys $
        head $
        dropWhile (not . journeyDone) $ iterate journeyFrom $ startState v
   in Map.fromList $ map (\v -> (v, findFastestOne v)) allValves

newRiver valves =
  let allValves = [(valveName v, flowRate v) | v <- Map.elems valves]
      vNames = Map.keys valves
      usefulValves =
        filter
          (\v -> flowRate (Map.findWithDefault noValve v valves) > 0)
          vNames
      journeyMap = findFastest valves
   in RiverState
        { atValve = "AA"
        , timeLeft = 30
        , cFlow = 0
        , stillClosed =
            Map.fromList $
            map
              (\v -> (v, flowRate $ Map.findWithDefault noValve v valves))
              usefulValves
        , journeyMap = journeyMap
        }

-- Find the available moves from a river state
availableMoves rv@RiverState { atValve = av
                             , timeLeft = t
                             , stillClosed = closed
                             , journeyMap = jm
                             } =
  let maybeMoves = Map.keys closed
      distances = Map.findWithDefault Map.empty av jm
      -- Note the time offset as we need an extra minute to open the valve
   in filter (\v -> Map.findWithDefault t v distances < (t - 2)) maybeMoves

riverMove rv@RiverState { atValve = av
                        , timeLeft = t
                        , stillClosed = closed
                        , journeyMap = jm
                        , cFlow = cf
                        } vTo =
  let maybeMoves = Map.keys closed
      distances = Map.findWithDefault Map.empty av jm
      distance = Map.findWithDefault 0 vTo distances
      flowForValve = Map.findWithDefault 0 vTo closed
      newTime = t - distance - 1
      newFlow = cf + (newTime * flowForValve) -- We get the new flow from opening the valve
      newClosed = Map.delete vTo closed
   in rv
        { atValve = vTo
        , timeLeft = newTime
        , stillClosed = newClosed
        , cFlow = newFlow
        }

-- Build a map of fastest paths between each
valveMap fname = do
  ls <- T.lines <$> TIO.readFile fname
  let valves = rights $ map (parseOnly pValve) ls
  return $ Map.fromList [(valveName v, v) | v <- valves]

data RiverStepState =
  RiverStepState
    { workingStates :: [RiverState]
    , allStates     :: [RiverState]
    }

stepRiverSS rss@RiverStepState {workingStates = ws, allStates = as} =
  let oneStep rs =
        let moves = availableMoves rs
         in map (riverMove rs) moves
      newStates = concatMap oneStep ws
   in RiverStepState {workingStates = newStates, allStates = as <> newStates}

part1 fname = do
  valves <- valveMap fname
  let startingRiver = newRiver valves
  let riverSteps =
        iterate
          stepRiverSS
          RiverStepState {workingStates = [startingRiver], allStates = []}
  let finalStates =
        allStates $ head $ dropWhile (not . null . workingStates) riverSteps
      -- = allStates . head $ drop 1 riverSteps
  let flows = map cFlow finalStates
  return $ maximum flows
