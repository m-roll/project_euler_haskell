#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import Data.Foldable (find)

main :: IO ()
main = print . fmap fst $ find (hasDigits 1000 . snd) (zip [1 ..] fibs)

hasDigits :: Int -> Integer -> Bool
hasDigits m n = n `div` (10 ^ (m - 1)) > 0

-- cool solution. trying to not use general recursion
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs