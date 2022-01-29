#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [data-ordlist])" -i runhaskell

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (find, intersect)
import Data.List.Ordered (isect)
import Text.XHtml (small)

-- as clean as this looks, it's pretty slow. Probably worth re-visiting
main :: IO ()
main = print $ head (foldl1 isect (map multiples [20, 19 .. 1]))

multiples :: Int -> [Int]
multiples n = [n, (2 * n) ..]
