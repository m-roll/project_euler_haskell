#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Data.List.Split (splitPlaces)

main :: IO ()
main = print . length $ filter ((==) sunday . head) (drop 12 dates)

dates :: [[Int]]
dates = splitPlaces (concatMap year [1900 .. 2000]) days

year :: Int -> [Int]
year y = map (monthLength y) [1 .. 12]

sunday :: Int
sunday = 7

days :: [Int]
days = cycle [1 .. 7]

monthLength :: Int -> Int -> Int
monthLength _ 1 = 31
monthLength year 2
  | isLeapYear = 29
  | otherwise = 28
  where
    isLeapYear = ((year `rem` 4) == 0) && ((year `rem` 100) /= 0 || (year `rem` 400) == 0)
monthLength _ 3 = 31
monthLength _ 4 = 30
monthLength _ 5 = 31
monthLength _ 6 = 30
monthLength _ 7 = 31
monthLength _ 8 = 31
monthLength _ 9 = 30
monthLength _ 10 = 31
monthLength _ 11 = 30
monthLength _ 12 = 31
monthLength _ _ = error "invalid month"