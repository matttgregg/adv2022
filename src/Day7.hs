{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import           Control.Monad
import           Control.Monad.State
import           Data.List           (sortOn)
import qualified Data.Map            as Map
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

data SystemItem
  = FileItem T.Text
  | DirItem T.Text
  deriving (Show)

data FileSystem =
  FileSystem
    { fileSizes   :: Map.Map T.Text Int
    , dirContents :: Map.Map T.Text [SystemItem]
    }
  deriving (Show)

data ReaderState =
  ReaderState
    { reversedDir :: [T.Text]
    , fSystem     :: FileSystem
    }

data ShellCommand
  = CallDir T.Text
  | LS
  | LSFile T.Text Int
  | LSDir T.Text

newReader :: ReaderState
newReader =
  ReaderState
    { reversedDir = []
    , fSystem = FileSystem {fileSizes = Map.empty, dirContents = Map.empty}
    }

currDirName :: ReaderState -> T.Text
currDirName = dirName . reverse . reversedDir
  where
    fixRoot t =
      if t == ""
        then "/"
        else "/" <> t
    dirName = fixRoot . T.intercalate "/"

process :: ShellCommand -> State ReaderState ()
-- The variations on call dir, just change the current directory.
process (CallDir "/") = do
  state <- get
  put state {reversedDir = []}
process (CallDir "..") = do
  state <- get
  let newDir = tail $ reversedDir state
  put state {reversedDir = newDir}
process (CallDir d) = do
  state <- get
  let newDir = d : reversedDir state
  put state {reversedDir = newDir}
-- The 'ls', make sure we don't re-read a directory
process LS = do
  state <- get
  let currDir = currDirName state
  let currFSystem = fSystem state
  let newDirContents = Map.insert currDir [] (dirContents currFSystem)
  let newFSystem = currFSystem {dirContents = newDirContents}
  put state {fSystem = newFSystem}
-- For a ls file command, add to the current directory and note the size
process (LSFile f sz) = do
  state <- get
  let currDir = currDirName state
  let currFSystem = fSystem state
  let fullFileName =
        if currDir == "/"
          then "/" <> f
          else currDir <> "/" <> f
  let newDirContents =
        Map.insertWith
          mappend
          currDir
          [FileItem fullFileName]
          (dirContents currFSystem)
  let newFileSizes = Map.insert fullFileName sz (fileSizes currFSystem)
  let newFSystem =
        currFSystem {dirContents = newDirContents, fileSizes = newFileSizes}
  put state {fSystem = newFSystem}
-- For a ls dir command, add to the current directory
process (LSDir d) = do
  state <- get
  let currDir = currDirName state
  let currFSystem = fSystem state
  let fullDirName =
        if currDir == "/"
          then "/" <> d
          else currDir <> "/" <> d
  let newDirContents =
        Map.insertWith
          mappend
          currDir
          [DirItem fullDirName]
          (dirContents currFSystem)
  let newFSystem = currFSystem {dirContents = newDirContents}
  put state {fSystem = newFSystem}

commandFromText :: T.Text -> ShellCommand
commandFromText = commandFromWords . T.words
  where
    commandFromWords ["$", "cd", d] = CallDir d
    commandFromWords ["$", "ls"]    = LS
    commandFromWords ["dir", d]     = LSDir d
    commandFromWords [sz, f]        = LSFile f (read $ T.unpack sz)

runSession :: [T.Text] -> State ReaderState FileSystem
runSession session = do
  put newReader
  -- Process the commands
  forM_ session (process <$> commandFromText)
  gets fSystem

type SizeCache = Map.Map T.Text Int

memoSizeOf :: FileSystem -> SystemItem -> State SizeCache Int
memoSizeOf fSystem (FileItem f) =
  return $ Map.findWithDefault 0 f (fileSizes fSystem)
memoSizeOf fSystem (DirItem d) = do
  cache <- get
  newTotal <-
    (if Map.member d cache
       then return $ Map.findWithDefault 0 d cache
       else let contents = Map.findWithDefault [] d (dirContents fSystem)
             in sum <$> mapM (memoSizeOf fSystem) contents)
  put $ Map.insert d newTotal cache
  return newTotal

sizeOfDir :: FileSystem -> T.Text -> Int
sizeOfDir fSystem d =
  let contents = Map.findWithDefault [] d (dirContents fSystem)
   in sum $ map sizeOfItem contents
  where
    sizeOfItem (FileItem f)  = Map.findWithDefault 0 f (fileSizes fSystem)
    sizeOfItem (DirItem nxt) = sizeOfDir fSystem nxt

runSessionFile :: String -> IO FileSystem
runSessionFile f = evalSession <$> TIO.readFile f
  where
    evalSession session = evalState (runSession $ T.lines session) newReader

evalSizes fSystem
  --SizedDirs $
 = map (\d -> (d, sizeOfDir fSystem d)) (Map.keys $ dirContents fSystem)

memoEvalSizes' :: FileSystem -> State SizeCache [Int]
memoEvalSizes' fSystem =
  mapM (memoSizeOf fSystem . DirItem) (Map.keys $ dirContents fSystem)
  where
    dirName (DirItem d) = d

memoEvalSizes fs = runState (memoEvalSizes' fs) Map.empty

newtype SizedDirs =
  SizedDirs [(T.Text, Int)]
  deriving (Show)

part1 :: String -> IO Int
part1 f = do
  (_, dirSizes) <- memoEvalSizes <$> runSessionFile f
  let sizedDirs =
        map (\k -> (k, Map.findWithDefault 0 k dirSizes)) (Map.keys dirSizes)
  let smallest = filter ((<= 100000) . snd) sizedDirs
  --forM_ sizedDirs (\(dr, sz) -> putStrLn (T.unpack dr <> ":" <> show sz))
  --putStrLn "\nSmallest directories::"
  --forM_ smallest (\(dr, sz) -> putStrLn (T.unpack dr <> ":" <> show sz))
  return $ sum $ map snd smallest

part2 :: String -> IO Int
part2 f = do
  (_, dirSizes) <- memoEvalSizes <$> runSessionFile f
  let sizedDirs =
        map (\k -> (k, Map.findWithDefault 0 k dirSizes)) (Map.keys dirSizes)
  let totalUsed = Map.findWithDefault 0 "/" dirSizes
  --putStrLn $ "Total Space used (/70,000,000) = " <> show totalUsed
  let usableSpace = 40000000
  let overuse = totalUsed - usableSpace
  --putStrLn $ "Overuse: " <> show overuse
  let bigEnough = sortOn snd $ filter ((>= overuse) . snd) sizedDirs
  let bestCandidate = head bigEnough
  {-putStrLn $
    "Best Candidate : " <>
    T.unpack (fst bestCandidate) <> " : " <> show (snd bestCandidate)
    -}
  return $ snd bestCandidate
