#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- reading that I don't understand, about htis subject: https://www.mlwright.org/docs/cycles.pdf

import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Ratio (denominator)
import Data.Text.Lazy.Builder.Int (decimal)

main :: IO ()
main = print . denominator $ maximumBy (comparing decimalCycleLength) unitFractions

--main = print . take 10 $ map decimalCycleLength unitFractions

--main = print $ decimalCycleLength (1 / 7)

unitFractions :: [Rational]
unitFractions = map (1 /) [2 .. 1000]

decimalCycleLength :: Rational -> Int
decimalCycleLength fr
  | d `mod` 5 == 0 || d `mod` 2 == 0 = 1
  | otherwise = head $ filter (\n -> 1 == 10 ^ n `mod` d) [1 ..]
  where
    d = denominator fr