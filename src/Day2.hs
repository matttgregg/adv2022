module Day2 where

import           Data.List.Split (splitOn)

data RPSShape
  = Rock
  | Paper
  | Scissors
  deriving (Show)

data RPSResult
  = Win
  | Lose
  | Draw
  deriving (Show)

scoreShape :: RPSShape -> Int
scoreShape Rock     = 1
scoreShape Paper    = 2
scoreShape Scissors = 3

scoreResult :: RPSResult -> Int
scoreResult Win  = 6
scoreResult Lose = 0
scoreResult Draw = 3

resolveGame :: RPSShape -> RPSShape -> RPSResult
resolveGame Rock Paper     = Win
resolveGame Rock Scissors  = Lose
resolveGame Paper Scissors = Win
resolveGame Paper Rock     = Lose
resolveGame Scissors Rock  = Win
resolveGame Scissors Paper = Lose
resolveGame _ _            = Draw

scoreGame :: RPSShape -> RPSShape -> Int
scoreGame theirs mine =
  (scoreShape mine) + (scoreResult $ resolveGame theirs mine)

readShape :: String -> RPSShape
readShape "X" = Rock
readShape "A" = Rock
readShape "Y" = Paper
readShape "B" = Paper
readShape "Z" = Scissors
readShape "C" = Scissors
readShape _   = Rock

responseTo :: RPSShape -> String -> RPSShape
responseTo shape "Y"    = shape
responseTo Rock "X"     = Scissors
responseTo Rock "Z"     = Paper
responseTo Paper "X"    = Rock
responseTo Paper "Z"    = Scissors
responseTo Scissors "X" = Paper
responseTo Scissors "Z" = Rock

scoreFromString :: String -> Int
scoreFromString game = scoreGame theirs mine
  where
    (theirs:mine:_) = map readShape $ splitOn " " game

scoreResponseFromString :: String -> Int
scoreResponseFromString game = scoreGame theirs mine
  where
    (t:m:_) = splitOn " " game
    theirs = readShape t
    mine = responseTo theirs m

part1 :: String -> IO Int
part1 file = do
  raw <- readFile file
  let scores = sum . map scoreFromString $ lines raw
  return scores

part2 :: String -> IO Int
part2 file = do
  raw <- readFile file
  let scores = sum . map scoreResponseFromString $ lines raw
  return scores
