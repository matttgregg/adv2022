module Day6 where

import qualified Data.ByteString as B
import           Data.Word       (Word8)

allDifferent ws =
  all
    (\ix ->
       let toCheck = ws !! (ix - 1)
        in notElem toCheck (drop ix ws))
    [1 .. (length ws)]

differentAt lngth t ix = allDifferent $ map (t !!) [(ix - lngth) .. (ix - 1)]

firstKey lngth t = head canBeKey
  where
    canBeKey = filter (differentAt lngth t) [lngth .. (length t)]

part1 fName = firstKey 4 . B.unpack <$> B.readFile fName

part2 fName = firstKey 14 . B.unpack <$> B.readFile fName
