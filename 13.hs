#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- SO this one is easy with the Integer type, since we can have arbitrary sized integers in Haskell.
-- BUT I'm going to try learning some math here and pretending we care about overflow.
-- edit: the math version is boring it's just integer division by some constant

main :: IO ()
main = print . take 10 . show . sum . map read . lines =<< getContents
