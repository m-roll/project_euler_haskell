#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print solution

solution :: Int
solution = nthChampernowne 1 * nthChampernowne 10 * nthChampernowne 100 * nthChampernowne 1000 * nthChampernowne 10000 * nthChampernowne 100000 * nthChampernowne 1000000

nthChampernowne :: Int -> Int
nthChampernowne n = champernowneDigits !! (n - 1)

champernowneDigits :: [Int]
champernowneDigits = map (read . pure) (concatMap show [1..])