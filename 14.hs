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
  | n `Map.member` cache = Just cache
  | otherwise = do
    newCache <- collatzLengths cache nextInSeq
    nextDepth <- Map.lookup nextInSeq newCache
    pure $ insert n (nextDepth + 1) newCache
  where
    nextInSeq = if even n then n `div` 2 else 3 * n + 1
