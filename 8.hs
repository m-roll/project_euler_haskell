#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Char (digitToInt)
import Data.List (tails)
import Data.Traversable (sequenceA)

-- pipe 8.in into this program like `cat 8.in | ./8.hs'

main :: IO ()
main = do
  inputString <- getContents
  let answer = largestProductInSeries (map digitToInt (filter ('\n' /=) inputString)) 13
   in print answer

largestProductInSeries :: [Int] -> Int -> Int
largestProductInSeries series len = maximum (map product (windows len series))

-- sliding window from https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails