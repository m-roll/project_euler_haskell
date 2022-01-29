#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print . last $ primeFactors 600851475143

primeFactors :: Int -> [Int]
primeFactors = filter isPrime . factors

isPrime :: Int -> Bool
isPrime = null . tail . factors

factors :: Int -> [Int]
factors n = filter ((==) 0 . mod n) (numsLessThanSqrt n)

numsLessThanSqrt :: Int -> [Int]
numsLessThanSqrt n = takeWhile (\x -> x ^ 2 <= n) [1 ..]
