#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- use the boxed Integer type for unbounded ints

main :: IO ()
main = print . sum . map (read . pure) . show $ fact 100

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)