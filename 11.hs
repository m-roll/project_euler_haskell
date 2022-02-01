#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.ByteString (foldl)
import Data.Function ((&))
import Data.List (tails, transpose)
import Data.List.Split (divvy)

-- probably should have just used a Matrix library ( I think Ed has one )
-- since I'm essentially just doing matrix manipulations. Learned this
-- stuff in Linear Algebra

type Row a = [a]

-- Grid is assumed to always be a rectangle (basically just a matrix). Could use vectors to enforce this
type Grid a = [Row a]

type LinearGridTraversal a = Grid a -> [[a]]

type RectangeGridTraversal a = Grid a -> [Grid a]

-- parameters from the problem
nn = 4

gridsize = 20

main :: IO ()
main = print . maximum . map product . traverseGridLines nn . input =<< getContents

-- there has got to be a more semantic way to do this
linearTraversals :: Int -> [LinearGridTraversal a]
linearTraversals n = map (n &) [traverseHorizonalSegments, traverseVerticalSegments, traverseRightDiagonal, traverseLeftDiagonal]

traverseGridLines :: Int -> LinearGridTraversal a
traverseGridLines n grid = concatMap (grid &) (linearTraversals n)

--interesting learning: =<< and concatMap seem to be equivalent?
traverseHorizonalSegments :: Int -> LinearGridTraversal a
traverseHorizonalSegments n = (=<<) (windows n)

traverseVerticalSegments :: Int -> LinearGridTraversal a
traverseVerticalSegments n xs = traverseHorizonalSegments n (gridRotate90 xs)

traverseRightDiagonal :: Int -> LinearGridTraversal a
traverseRightDiagonal n xs = map gridDiagonal $ gridWindows n xs

traverseLeftDiagonal :: Int -> LinearGridTraversal a
traverseLeftDiagonal n = traverseRightDiagonal n . gridMirrorVertically

gridDiagonal :: Grid a -> [a]
gridDiagonal ([] : _) = []
gridDiagonal [] = []
gridDiagonal ((corner : _) : rest) = corner : gridDiagonal (map tail rest)

-- all nxn grid windows. kind of squinty here. Use these boxes for diagonals.
gridWindows :: Int -> RectangeGridTraversal a
gridWindows n xs = windows n xs >>= (transpose . map (windows n))

-- grid utilities
gridRotate90 :: Grid a -> Grid a
gridRotate90 = transpose

gridMirrorVertically :: Grid a -> Grid a
gridMirrorVertically = map reverse

-- sliding window from https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

input :: String -> Grid Int
input = divvy gridsize gridsize . map read . words