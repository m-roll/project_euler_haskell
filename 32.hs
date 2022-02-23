#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

-- this works but kind of sucks. the problem space would be much smaller if I instead started with potential products
-- instead of the string manipulation approach
import Data.List (nub, permutations)
import Data.List.Split (splitPlaces)

main :: IO ()
main = print . sum . nub . map (\[x, y, z] -> z) . filter (\[x, y, z] -> x < y) . filter (\[x, y, z] -> x * y == z) $ digitPerms

--
--main = print . filter (\[x, y, z] -> x * y == z) $ digitPerms

digitPerms :: [[Int]]
digitPerms = (concatMap . map . map) read digitPermsStr

digitPermsStr :: [[[String]]]
digitPermsStr = recombine <$> permutations "123456789"

recombine :: String -> [[String]]
recombine s = map (`splitPlaces` s) stringPartitions

stringPartitions :: [[Int]]
stringPartitions = [1 .. 7] >>= \num -> map (\num2 -> [num, num2, 9 - num - num2]) [1 .. (8 - num)]