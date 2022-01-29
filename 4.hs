#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print . maximum $ filter isPalindrome (allProducts 100 999)

isPalindrome :: Int -> Bool
isPalindrome n =
  let nStr = show n
   in nStr == reverse nStr

allProducts :: Int -> Int -> [Int]
allProducts m n = map (*) [m .. n] <*> [m .. n]