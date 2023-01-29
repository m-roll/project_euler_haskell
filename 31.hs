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
-- - a list of other coin values, sorted in descending order
-- Return all unique combinations of coins to create the remainder. Use the greatest available
--    coin value (c) to deduce how many different ways (c) can fit into remainder, then recurse.

coinsums :: Pence -> [CoinValue] -> [[CoinValue]]

-- Base case: There is 1 way to divide up the remainder 0, and that is with no coins.
coinsums 0 _ = [[]]

-- Base case: There are 0 ways to divide up a remainder using 0 types of coins.
coinsums _ [] = []

-- Recursive case.
coinsums remainder (coin:restCoins)
  -- can't use this coin for the remainder anymore, skip this coin.
  | coin > remainder = coinsums remainder restCoins
  -- Otherwise, append this coin to the recursive case.
  -- Note: ++ represents a choice. We can choose to either use this coin, or skip using
  --  this coin and try to subdivide with other coins.
  --  fmap represents the transformation of our answer where we add the coin to our
  --  soln list.
  | otherwise = (coin :) <$> coinsums (remainder - coin) (coin:restCoins) ++ coinsums remainder restCoins