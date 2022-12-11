{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import           Data.List    (nub)
import qualified Data.Map     as Map
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Debug.Trace  (trace)

newtype Pt =
  Pt (Int, Int)
  deriving (Eq, Ord, Show)

data Rope =
  Rope
    { hd      :: Pt
    , knots   :: [Pt]
    , crossed :: Map.Map Pt ()
    }
  deriving (Show)

newRope :: Int -> Rope
newRope knotCount =
  Rope
    { hd = Pt (0, 0)
    , knots = replicate knotCount $ Pt (0, 0)
    , crossed = Map.fromList [(Pt (0, 0), ())]
    }

mvHead :: Rope -> Pt -> Rope
mvHead rp@(Rope {hd = Pt (hdx, hdy)}) (Pt (mvx, mvy)) =
  rp {hd = Pt (hdx + mvx, hdy + mvy)}

collapseRope :: Rope -> Rope
collapseRope rp@Rope {hd = h, knots = ks} = rp {knots = newKnots}
  where
    newKnots = drop 1 $ reverse $ foldl doKnot [h] ks
    doKnot hds k = collapseKnot (head hds) k : hds

collapseKnot :: Pt -> Pt -> Pt
collapseKnot (Pt (hdx, hdy)) tl@(Pt (tlx, tly))
  | hdx == tlx && abs (hdy - tly) > 1 = Pt (tlx, approach tly hdy)
  | hdy == tly && abs (hdx - tlx) > 1 = Pt (approach tlx hdx, tly)
  | abs (hdx - tlx) > 1 || abs (hdy - tly) > 1 =
    Pt (approach tlx hdx, approach tly hdy)
  | otherwise = tl
        -- appraoch shifts finds a value from the first value one step closer to the second
  where
    approach p1 p2
      | p2 > p1 = p1 + 1
      | p2 < p1 = p1 - 1
      | otherwise = p1

markRope :: Rope -> Rope
markRope rp@Rope {knots = ks, crossed = c} =
  rp {crossed = Map.insert (last ks) () c}

runOnce :: Rope -> Char -> Rope
runOnce rp 'R' = markRope $ collapseRope $ mvHead rp $ Pt (1, 0)
runOnce rp 'L' = markRope $ collapseRope $ mvHead rp $ Pt (-1, 0)
runOnce rp 'U' = markRope $ collapseRope $ mvHead rp $ Pt (0, 1)
runOnce rp 'D' = markRope $ collapseRope $ mvHead rp $ Pt (0, -1)

runStep :: Rope -> Char -> Int -> Rope
runStep rp dir ct = iterate (`runOnce` dir) rp !! ct

runLine :: Rope -> T.Text -> Rope
runLine rp l = runStep rp dir ct
  where
    ws = T.words l
    dir = T.head $ head ws
    ct = read $ T.unpack $ ws !! 1

runFile :: Int -> String -> IO Int
runFile kCount f = do
  contents <- TIO.readFile f
  let ls = T.lines contents
  let rp = foldl runLine (newRope kCount) ls
  return $ covered rp
  where
    covered Rope {crossed = crs} = length $ Map.keys crs

part1 :: String -> IO Int
part1 = runFile 1

part2 :: String -> IO Int
part2 = runFile 9
