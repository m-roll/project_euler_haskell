#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell
import Data.List (sortOn, groupBy)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)

-- Problem 39 solved with Euclid's formula
main :: IO ()
main = print $ perimeter <$> maximumBy (comparing length) (groupBy ((==) `on` perimeter) (sortOn perimeter (pythagoreanTrianglesLessThanP 1000)))

data Triangle = Triangle Integer Integer Integer deriving (Show)

perimeter :: Triangle -> Integer
perimeter (Triangle a b c) = a + b + c

pythagoreanTrianglesLessThanP :: Integer -> [Triangle]
pythagoreanTrianglesLessThanP lim = pythagoreanTrianglePrims >>= takeWhile ((< lim) . perimeter) . ktriangles 

ktriangles :: Triangle -> [Triangle]
ktriangles (Triangle a b c) = fmap (\k -> Triangle (k*a) (k*b) (k*c)) [1..500]

-- https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
pythagoreanTrianglePrims :: [Triangle]
pythagoreanTrianglePrims = [Triangle a b c | 
                                         n <- [1..23],
                                         m <- [n..23],
                                         even n || even m,
                                         coprime m n,
                                         let a = m^2 - n^2,
                                         let b = 2 * m * n,
                                         let c = m^2 + n^2]

coprime :: Integer -> Integer -> Bool
coprime = ((1 ==) .) . gcd