#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- this one seems lame so I'm going to come back to it. lots of questions already ask about divisors.
main :: IO ()
main = print "foobar"
