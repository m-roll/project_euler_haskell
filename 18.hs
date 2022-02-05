#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

--Note: this is a working solution for 67 too. the file "67" is a symlink to here.

data Parents a = None | Single a | Double a a

main :: IO ()
main = print . getMaxPath . input =<< getContents

getMaxPath :: [[Int]] -> Int
getMaxPath rows = maximum (foldl (\parents row -> maxRowPaths (rowParents parents row)) [] rows)

rowParents :: [Int] -> [Int] -> [([Int], Int)]
rowParents [] [first] = [([0], first)] -- top of pyramid
rowParents (firstParent : parents) (firstVal : rest) = ([firstParent], firstVal) : f (firstParent : parents) rest
  where
    f :: [Int] -> [Int] -> [([Int], Int)]
    f (p1 : p2 : ps) (v : vs) = ([p1, p2], v) : f (p2 : ps) vs
    f [p] [v] = [([p], v)]
    f _ _ = []
rowParents _ _ = [] -- should not happen

maxRowPaths :: [([Int], Int)] -> [Int]
maxRowPaths = map (\(parents, value) -> getMaxParent parents + value)

getMaxParent :: [Int] -> Int
getMaxParent [] = 0
getMaxParent parents = maximum parents

input :: String -> [[Int]]
input = map (map read . words) . lines