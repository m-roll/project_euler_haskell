#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print $ sumSquareDifference 100

sumSquareDifference :: Int -> Int
sumSquareDifference n =
  let seq = [1 .. n]
   in (sum seq ^ 2) - sum (map (^ 2) seq)