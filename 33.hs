#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [split])" -i runhaskell

import Data.List (nub)
import Data.Ratio (Ratio, denominator, (%))

-- could restrict problem space so dont need to filter <1
main :: IO ()
main = print . denominator . product . filter (< 1) . map doublePairToRational . filter nonTrivial $ filter canFakeReduce allMagicRatiosWithSharedDigit

nonTrivial :: ((Integer, Integer), (Integer, Integer)) -> Bool
nonTrivial (n@(num1, num2), d@(den1, den2))
  | n == d = False
  | num2 == 0 && den2 == 0 = False
  | otherwise = True

canFakeReduce :: ((Integer, Integer), (Integer, Integer)) -> Bool
canFakeReduce doublePair =
  snd (fakeReduce doublePair) /= 0 && (doublePairToRational doublePair == (pairToRational . fakeReduce) doublePair)

pairToRational :: (Integer, Integer) -> Rational
pairToRational = uncurry (%)

doublePairToRational :: ((Integer, Integer), (Integer, Integer)) -> Rational
doublePairToRational (num, dem) = f num % f dem
  where
    f (p1, p2) = p1 * 10 + p2

fakeReduce :: ((Integer, Integer), (Integer, Integer)) -> (Integer, Integer)
fakeReduce ((num1, num2), (den1, den2))
  | num1 == den1 = (num2, den2)
  | num1 == den2 = (num2, den1)
  | num2 == den1 = (num1, den2)
  | num2 == den2 = (num1, den1)
  | otherwise = error "Can't fake reduce"

allMagicRatiosWithSharedDigit :: [((Integer, Integer), (Integer, Integer))]
allMagicRatiosWithSharedDigit = do
  num1 <- [1 .. 9]
  num2 <- [1 .. 9]
  let denomsMatchingFirst = (,) <$> [num1, num2] <*> [0 .. 9]
  let denomsMatchingSecond = (,) <$> [1 .. 9] <*> [num1, num2]
  denom <- denomsMatchingFirst ++ denomsMatchingSecond
  pure ((num1, num2), denom)