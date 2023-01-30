#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print . length $ parts 200 coins

parts :: Int -> [[Int]] -> Int -> [Int] -> [[Int]]
parts sumAcc acc 0 ps = acc
parts sumAcc acc n ps = [[]]

-- good problem for dynamic programming
-- this works, but is far too slow without some kind of memoization
--parts n ps = concatMap (\part -> map (part :) (parts (n - part) ps)) (filter (<= n) ps)

coins :: [Int]
coins = [100, 50, 20, 5, 2, 1]

-- start with n, non-deterministically subtract some amount from it (that is < n), nondeterministically add that value to the list (), recur. Base stop recurring when n=0
-- can this be done with a recursive abstraction?