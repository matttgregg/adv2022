{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import           Data.List.Index (indexed)
import           Data.List.Split (chunksOf)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

data Prog =
  Prog
    { cycles :: [Int]
    , regX   :: Int
    }
  deriving (Show)

newProg :: Prog
newProg = Prog {cycles = [1], regX = 1}

runLine :: Prog -> T.Text -> Prog
runLine prog@Prog {cycles = cs, regX = x} "noop" = prog {cycles = x : cs}
runLine prog l =
  let ws = T.words l
   in runWords prog ws
  where
    runWords prog@Prog {cycles = cs, regX = x} ["addx", val] =
      prog {cycles = x : x : cs, regX = x + read (T.unpack val)}
    runWords prog _ = prog

runLines :: Prog -> [T.Text] -> Prog
runLines = foldl runLine

-- Run frequencies executes the program and finds frequencies at 20, 60, .. 220
runFrequencies :: Prog -> [T.Text] -> [Int]
runFrequencies prog ls = cyclesOfProg (runLines prog ls)
  where
    cyclesOfProg Prog {cycles = cs} =
      map (\ix -> ix * (reverse cs !! ix)) [20,60 .. 220]

imageFromCycles :: [Int] -> [Bool]
imageFromCycles cs = zipWith isVisible (drop 1 cs) scans
  where
    scans = map (`mod` 40) [0 .. 239]
    isVisible x y = abs (x - y) <= 1

printImage :: [Bool] -> IO ()
printImage im = do
  let pxs =
        map
          (\px ->
             if px
               then "#"
               else " ")
          im
  let ls = map T.concat $ chunksOf 40 pxs
  mapM_ print ls

part1 :: FilePath -> IO Int
part1 fname = sum . runFrequencies newProg . T.lines <$> TIO.readFile fname

part2 :: FilePath -> IO ()
part2 fname =
  TIO.readFile fname >>=
  printImage . imageFromCycles . reverse . cycles . runLines newProg . T.lines
