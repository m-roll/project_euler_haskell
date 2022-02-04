#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import Control.Monad (foldM)
import Data.Foldable (Foldable (toList), maximumBy)
import Data.Map (Map, empty, insert, lookup, member, singleton, toList, union)
import qualified Data.Map as Map
import Data.Ord (comparing)

-- some thoughts:
-- can we memoize collatz by running it in reverse?
-- - If yes, how do we keep track of the length of the chain?

-- looks like the reverse method is stupid. need to just memoize with maybe a map.

-- TODO: How can I clean up this code? It looks quite ugly.

main :: IO ()
main = print $ fmap (maximumBy (comparing snd) . Map.toList) collatzMap

collatzMap :: Maybe (Map Int Int)
collatzMap = foldM collatzLengths (singleton 1 1) [1 .. 1000000]

collatzLengths :: Map Int Int -> Int -> Maybe (Map Int Int)
collatzLengths cache n
  | n == 1 = Just cache
  | otherwise = case Map.lookup n cache of
    Nothing -> collatzLengths cache nextInSeq >>= \newCache -> fmap (\nextDepth -> insert n (nextDepth + 1) newCache) (Map.lookup nextInSeq newCache)
    Just _ -> Just cache
  where
    evenCollatz = n `div` 2
    oddCollatz = 3 * n + 1
    nextInSeq = if even n then evenCollatz else oddCollatz