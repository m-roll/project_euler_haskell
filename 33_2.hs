#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Data.Ratio (denominator, (%))

-- three different digits (since at least one is shared)
main :: IO ()
main =
  print . denominator . product $
    [ (a * 10 + b) % (b * 10 + c)
      | a <- [1 .. 9],
        b <- [1 .. 9],
        c <- [1 .. 9],
        a /= b && b /= c,
        canFakeReduce a b c
    ]
  where
    canFakeReduce :: Integer -> Integer -> Integer -> Bool
    canFakeReduce a b c = ((a * 10 + b) % (b * 10 + c)) == a % c