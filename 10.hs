#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print $ sum (takeWhile (< 2000000) primes)

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

union :: [[Integer]] -> [Integer]
union = foldr merge []
  where
    merge (x : xs) ys = x : merge' xs ys
    merge' (x : xs) (y : ys)
      | x < y = x : merge' xs (y : ys)
      | x == y = x : merge' xs ys
      | x > y = y : merge' (x : xs) ys