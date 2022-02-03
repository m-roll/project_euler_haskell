#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import Data.List (group)

-- I got a lot of help on this one :S

main :: IO ()
main = print $ head (filter ((>= 500) . numDivisors) triangleNumbers)

posInts :: [Int]
posInts = [1 ..]

triangleNumbers :: [Int]
triangleNumbers = 1 : 3 : zipWith (+) (drop 2 posInts) (tail triangleNumbers)

-- numDivisors is wrong for the first 2 numbers, which doesn't matter.
numDivisors :: Int -> Int
numDivisors n = product $ map ((+) 1 . length) (group (primeFactors n))

primeFactors n = factor n primes
  where
    factor n (p : ps)
      | p * p > n = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p : ps)
      | otherwise = factor n ps

-- sieve taken from https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes = 2 : ([3 ..] `minus` composites)
  where
    composites = union [multiples p | p <- primes]

multiples :: (Num b, Enum b) => b -> [b]
multiples n = map (n *) [n ..]

minus :: Ord a => [a] -> [a] -> [a]
(x : xs) `minus` (y : ys)
  | x < y = x : (xs `minus` (y : ys))
  | x == y = xs `minus` ys
  | x > y = (x : xs) `minus` ys

union :: [[Int]] -> [Int]
union = foldr merge []
  where
    merge (x : xs) ys = x : merge' xs ys
    merge' (x : xs) (y : ys)
      | x < y = x : merge' xs (y : ys)
      | x == y = x : merge' xs ys
      | x > y = y : merge' (x : xs) ys