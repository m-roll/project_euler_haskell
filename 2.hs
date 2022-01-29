#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

{-# LANGUAGE OverloadedStrings #-}

main :: IO ()
main = print . sum . filter even $ takeWhile (< 4000000) fib

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)