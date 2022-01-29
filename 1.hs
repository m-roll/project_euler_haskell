#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

{-# LANGUAGE OverloadedStrings #-}

main :: IO ()
main = print $ sumrange 1000

sumrange :: Int -> Int
sumrange n = sum $ filter (multipleOfAny [3, 5]) [1 .. n -1]

multipleOfAny :: [Int] -> Int -> Bool
multipleOfAny ns n = any ((==) 0 . mod n) ns
