#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Data.List (permutations, sort)

main :: IO ()
main = print prob24

prob24 :: String
prob24 = probPermutations !! 999999 -- 1 indexed

probPermutations :: [String]
probPermutations = sort (permutations "0123456789")
