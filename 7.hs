#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print $ primes !! 10000 -- zero indexed

primes :: [Int]
primes = sieve [2 ..]

-- Note: this is the "incorrect" sieve of eratosthenes as pointed out in
-- [this paper](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf)
sieve :: Integral a => [a] -> [a]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]