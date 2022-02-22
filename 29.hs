#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import Data.List (nub)

main :: IO ()
main = print . length $ nub ((^) <$> [2 .. 100] <*> [2 .. 100])