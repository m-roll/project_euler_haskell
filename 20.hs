#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- use the boxed Integer type for unbounded ints

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