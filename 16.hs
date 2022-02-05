#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- This is again super easy with boxed integers in Haskell

main :: IO ()
main = print $ sum (map (read . pure) (show (2 ^ 1000)))