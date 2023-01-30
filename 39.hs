#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell
import Data.Foldable (find, maximumBy)
import Data.Bifunctor
import Data.List (groupBy, sortOn)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.Ord (comparing)


main :: IO ()
main = print $ p <$> maximumBy (comparing length) (groupBy ((==) `on` p) (sortOn p rightTriangles))
-- this is pretty slow. Bottlebeck is rightTriangles filtering.

data Triangle = Triangle Integer Integer Integer deriving (Show)


-- Could insteead use euclid's formula for generating triplets.
rightTriangles :: [Triangle]
rightTriangles = catMaybes [rightTriangle a b | a <- [1..1000],
                                      b <- [a..1000]]

p :: Triangle -> Integer
p (Triangle a b c) = a + b + c

rightTriangle :: Integer -> Integer -> Maybe Triangle
rightTriangle a b =
     fmap toTriangle (find isSquareTriangle [b..r])
  where
    r = 1000 - a - b
    isSquareTriangle c = a^2 + b^2 == c^2
    toTriangle = Triangle a b