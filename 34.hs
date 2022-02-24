#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Control.Monad (ap)

-- 1000000 is an arbitrary cutoff that seems fine
main :: IO ()
main = print . sum $ filter isCurious [3 .. 1000000]

-- `ap` is cool
isCurious :: Integer -> Bool
isCurious = ap (==) (sum . map (fact . read . pure) . show)

fact :: Integer -> Integer
fact n = product [1 .. n]