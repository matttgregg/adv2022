{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import qualified Data.Array    ((!))
import qualified Data.Array    as Array
import qualified Data.Array.ST as ST
import           Data.List     (sort)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Debug.Trace   (trace)

data Monkey =
  Monkey
    { items      :: [Integer]
    , mOp        :: MOp
    , mTest      :: MTest
    , trueThrow  :: Integer
    , falseThrow :: Integer
    , busy       :: Integer
    }
  deriving (Show)

data MOp
  = MAdd Integer
  | MMult Integer
  | MSquare
  deriving (Show)

mworry :: MOp -> Integer -> Integer -> Integer
mworry (MAdd x) v relief  = (x + v) `div` relief
mworry (MMult x) v relief = (x * v) `div` relief
mworry MSquare v relief   = (v * v) `div` relief

newtype MTest =
  MDivisible Integer
  deriving (Show)

mcheck :: MTest -> Integer -> Bool
mcheck (MDivisible x) v = v `mod` x == 0

newMonkey :: Monkey
newMonkey =
  Monkey
    { items = []
    , mOp = MAdd 0
    , mTest = MDivisible 1
    , trueThrow = 0
    , falseThrow = 0
    , busy = 0
    }

--  Starting items: 79, 98
addItems :: Monkey -> T.Text -> Monkey
addItems oldM l =
  let [_, itemPart] = T.splitOn ":" l
      items = map (read . T.unpack) (T.splitOn "," itemPart)
   in oldM {items = items}

--  Operation: new = old * 19
addOp :: Monkey -> T.Text -> Monkey
addOp oldM l =
  let [_, opPart] = T.splitOn "=" l
      toOp :: [T.Text] -> MOp
      toOp ["old", "*", "old"] = MSquare
      toOp ["old", "*", arg]   = MMult (read $ T.unpack arg)
      toOp ["old", "+", arg]   = MAdd (read $ T.unpack arg)
      toOp _                   = MAdd 0
      newOp = toOp $ T.words opPart
   in oldM {mOp = newOp}

--  Test: divisible by 23
addTest :: Monkey -> T.Text -> Monkey
addTest oldM l = oldM {mTest = MDivisible $ read d}
  where
    d = T.unpack $ T.words l !! 3

--    If true: throw to monkey 2
--    If false: throw to monkey 3
addIf :: Monkey -> T.Text -> Monkey
addIf oldM l =
  let ws = T.words l
      bSwitch = ws !! 1
      val = read $ T.unpack $ ws !! 5
   in fixMonkey oldM bSwitch val
  where
    fixMonkey om "true:" v  = om {trueThrow = v}
    fixMonkey om "false:" v = om {falseThrow = v}

data MonkeyReader =
  MonkeyReader
    { currentMonkey    :: Monkey
    , collectedMonkeys :: [Monkey]
    }

newMonkeyReader :: MonkeyReader
newMonkeyReader =
  MonkeyReader {currentMonkey = newMonkey, collectedMonkeys = []}

readMLine :: MonkeyReader -> T.Text -> MonkeyReader
readMLine mr@MonkeyReader {currentMonkey = cMonkey, collectedMonkeys = cMonkeys} l =
  let ws = T.words l
      l1 =
        if not (null ws)
          then head ws
          else ""
   in readMLine' l1 mr l
  where
    readMLine' :: T.Text -> MonkeyReader -> T.Text -> MonkeyReader
    readMLine' "Monkey" mr _ =
      mr {currentMonkey = newMonkey, collectedMonkeys = cMonkey : cMonkeys} -- Start a new monkey
    readMLine' "Starting" mr l = mr {currentMonkey = addItems cMonkey l}
    readMLine' "Operation:" mr l = mr {currentMonkey = addOp cMonkey l}
    readMLine' "Test:" mr l = mr {currentMonkey = addTest cMonkey l}
    readMLine' "If" mr l = mr {currentMonkey = addIf cMonkey l}
    readMLine' _ mr _ = mr

readMonkeys :: T.Text -> [Monkey]
readMonkeys txt =
  let MonkeyReader {currentMonkey = cMonkey, collectedMonkeys = cMonkeys} =
        foldl readMLine newMonkeyReader $ T.lines txt
   in drop 1 $ reverse $ cMonkey : cMonkeys

monkeyState :: MonkArray -> [T.Text]
monkeyState (MonkArray ms) = map monkeyState $ Array.elems ms
  where
    monkeyState Monkey {items = its} = T.unwords $ map (T.pack . show) its

newtype MonkArray =
  MonkArray (Array.Array Integer Monkey)
  deriving (Show)

-- Shift a single item
shiftOne :: Integer -> MonkArray -> Integer -> MonkArray
shiftOne idx ms@(MonkArray monkeys) itm =
  let active = monkeys Array.! idx
      worried = mworry (mOp active) itm 3
      tested = mcheck (mTest active) worried
      toMonkey =
        if tested
          then trueThrow active
          else falseThrow active
      oldToMonkey@Monkey {items = oitems} = monkeys Array.! toMonkey
      newToMonkey = oldToMonkey {items = oitems <> [worried]}
      {- trace
        ("Monkey " ++
         show idx ++
         " inspects " ++
         show itm ++
         " -> " ++
         show worried ++
         " test is " ++ show tested ++ " so throws to " ++ show toMonkey) -}
   in MonkArray $ monkeys Array.// [(toMonkey, newToMonkey)]

runMonkey :: MonkArray -> Integer -> MonkArray
runMonkey ms@(MonkArray monkeys) idx =
  let activeMonkey = monkeys Array.! idx
      toShift = items activeMonkey
      oldBusy = busy activeMonkey
      MonkArray newMonkeys = foldl (shiftOne idx) ms toShift
      newActive =
        activeMonkey {items = [], busy = oldBusy + toInteger (length toShift)}
   in MonkArray $ newMonkeys Array.// [(idx, newActive)]

runRound ms@(MonkArray monkeys) =
  foldl runMonkey ms [0 .. toInteger $ length monkeys - 1]

part1 fname = do
  monkeys <- readMonkeys <$> TIO.readFile fname
  let amonkeys =
        MonkArray $ Array.listArray (0, toInteger $ length monkeys - 1) monkeys
  let finalMonkeys@(MonkArray fMonkeys) = iterate runRound amonkeys !! 20
  mapM_ TIO.putStrLn $ monkeyState amonkeys
  mapM_ TIO.putStrLn $ monkeyState finalMonkeys
  let mbusiness = map busy $ Array.elems fMonkeys
  mapM_ (TIO.putStrLn . T.pack . show) mbusiness
  return $ product $ take 2 $ reverse $ sort mbusiness
