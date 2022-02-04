#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import Data.Foldable (Foldable (toList), maximumBy)
import Data.Map (Map, empty, insert, lookup, member, singleton, toList, union)
import qualified Data.Map as Map

-- some thoughts:
-- can we memoize collatz by running it in reverse?
-- - If yes, how do we keep track of the length of the chain?

-- looks like the reverse method is stupid. need to just memoize with maybe a map.

-- TODO: How can I clean up this code? It looks quite ugly.

main :: IO ()
main = print $ fmap (maximumBy (\(_, length) (_, length2) -> length `compare` length2) . Map.toList) collatzMap

collatzMap :: Maybe (Map Int Int)
collatzMap = foldl (\cacheM n -> cacheM >>= \cache -> collatzLengths cache n) (Just $ singleton 1 1) [1 .. 1000000]

collatzLengths :: Map Int Int -> Int -> Maybe (Map Int Int)
collatzLengths = collatzLengthsLess 1000000

collatzLengthsLess :: Int -> Map Int Int -> Int -> Maybe (Map Int Int)
collatzLengthsLess max cache n
  | n == 1 = Just cache
  | otherwise = case Map.lookup n cache of
    Nothing -> recur cache nextInSeq >>= \newCache -> Map.lookup nextInSeq newCache >>= \nextDepth -> Just (insert n (nextDepth + 1) newCache)
    Just _ -> Just cache
  where
    evenCollatz = n `div` 2
    oddCollatz = 3 * n + 1
    nextInSeq = if even n then evenCollatz else oddCollatz
    recur = collatzLengthsLess max