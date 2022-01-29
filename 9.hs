#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

import Data.List (find)
import Text.XHtml.Frameset (small)

-- Pretty slow and messy. Probably revisit and at least get rid of tuple stuff.

main :: IO ()
main = print $ fmap tripleProduct (find (\((a, as), (b, bs), (c, cs)) -> a + b + c == 1000) pythagTriplets)

tripleProduct :: ((Int, Int), (Int, Int), (Int, Int)) -> Int
tripleProduct ((a, as), (b, bs), (c, cs)) = a * b * c

pythagTriplets :: [((Int, Int), (Int, Int), (Int, Int))]
pythagTriplets = filter isPythagTriple ((,,) <$> smallSquares <*> smallSquares <*> smallSquares)

smallSquares :: [(Int, Int)]
smallSquares = takeWhile (\(x, xs) -> x < 1000) squares

squares :: [(Int, Int)]
squares = map (\x -> (x, x ^ 2)) [1 ..]

isPythagTriple :: ((Int, Int), (Int, Int), (Int, Int)) -> Bool
isPythagTriple ((a, as), (b, bs), (c, cs)) = as + bs == cs