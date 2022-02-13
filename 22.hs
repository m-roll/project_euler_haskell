#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Data.List (elemIndex, sort)
import Data.List.Split

main :: IO ()
main = print . fmap sum . nameScores . sort . names =<< getContents

names :: String -> [String]
names text = map (filter (/= '"')) (splitOn "," text)

nameScores :: [String] -> Maybe [Int]
nameScores names = fmap (zipWith (*) [1 ..]) (traverse score names)
  where
    score :: String -> Maybe Int
    score name = fmap sum (traverse letterScore name)

letterScore :: Char -> Maybe Int
letterScore c = fmap (+ 1) (elemIndex c ['A' .. 'Z'])