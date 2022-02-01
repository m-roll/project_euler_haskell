#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.ByteString (foldl)
import Data.List (tails, transpose)
import Data.List.Split (divvy)

-- probably should have just used a Matrix library ( I think Ed has one )
-- since I'm essentially just doing matrix manipulations. Learned this
-- stuff in Linear Algebra

type Row a = [a]

-- Grid is assumed to always be a rectangle (basically just a matrix). Could use vectors to enforce this
type Grid a = [Row a]

-- parameters from the problem
nn = 4

gridsize = 20

main :: IO ()
main = print . maximum . map product . traverseGridLines nn . input =<< getContents

traverseGridLines :: Int -> Grid a -> [[a]]
traverseGridLines n grid = concatMap (\f -> f n grid) [traverseHorizonalSegments, traverseVerticalSegments, traverseRightDiagonal, traverseLeftDiagonal]

traverseHorizonalSegments :: Int -> Grid a -> [[a]]
traverseHorizonalSegments n = concatMap $ windows n

traverseVerticalSegments :: Int -> Grid a -> [[a]]
traverseVerticalSegments n xs = traverseHorizonalSegments n (gridRotate90 xs)

traverseRightDiagonal :: Int -> Grid a -> [[a]]
traverseRightDiagonal n xs = map gridDiagonal $ gridWindows n xs

traverseLeftDiagonal :: Int -> Grid a -> [[a]]
traverseLeftDiagonal n = traverseRightDiagonal n . gridMirrorVertically

gridDiagonal :: Grid a -> [a]
gridDiagonal ([] : _) = []
gridDiagonal [] = []
gridDiagonal ((corner : _) : rest) = corner : gridDiagonal (map tail rest)

-- all nxn grid windows. kind of squinty here. Use these boxes for diagonals.
gridWindows :: Int -> Grid a -> [Grid a]
gridWindows n xs = concatMap (transpose . map (windows n)) (windows n xs)

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