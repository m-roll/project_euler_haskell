#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print . length $ solution

type Pence = Int
type CoinValue = Pence

-- All available coin values, sorted descending by value. Could also use a Set Pence,
-- but set min/max lookups are O(logn) instead of O(1), which we get when they are sorted
-- like this.
allCoins :: [CoinValue]
allCoins = [200, 100, 50, 20, 10, 5, 2, 1]

solution :: [[CoinValue]]
solution = coinsums 200 allCoins

-- Given a 
-- - total value (remaineder)
-- - a coin value (c)
-- - a list of other coin values, sorted in descending order and not containing coin value (c)
-- Return all unique combinations of coins to create the remainder. Use the current coin value
--   (c) to deduce how many different ways (c) can fit into remaineder, then recurse.

coinsums :: Pence -> [CoinValue] -> [[CoinValue]]

-- Base case: There is 1 way to divide up the remainder 0, and that is with no coins.
coinsums 0 _ = [[]]

-- Base case: There are 0 ways to divide up a remainder using 0 types of coins.
coinsums _ [] = []

-- Recursive case.
coinsums remainder (coin:restCoins)
  -- can't use this coin for the remainder anymore, skip.
  | coin > remainder = coinsums remainder restCoins
  -- Otherwise, append this coin to the recursive case.
  | otherwise = fmap (coin :) (coinsums (remainder - coin) (coin:restCoins)) ++ coinsums remainder restCoins