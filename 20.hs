#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- use the boxed Integer type for unbounded ints

-- NOTE: Learned some cool stuff here about tail recursion vs guarded recursion.
-- In Haskell, due to laziness, tail calls (at least in this example) can be
-- LESS efficient than guarded recursion due to thunked computations.
-- Guarded recursion can actually be more-performant memory-wise, so the
-- non-tail-recursive function may actually be the better answer here.
-- More info: https://stackoverflow.com/questions/13042353/does-haskell-have-tail-recursive-optimization

main :: IO ()
main = print . sum . map (read . pure) . show $ fact 100

-- non tail-recursive option
-- fact :: Integer -> Integer
-- fact 1 = 1
-- fact n = n * fact (n - 1)

-- tail-recursive

fact :: Integer -> Integer
fact n = f 1 1
  where
    f m acc
      | n == m = m * acc
      | otherwise = f (m + 1) acc * m