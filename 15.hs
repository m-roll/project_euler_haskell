#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad ((>=>))

-- I got outside help on this one. This solution makes sense if you think of the base cases.
-- i.e. why is the first row only ones? the first column should be only ones too, how does this identity hold?
-- What does the second row look like? How does this iterate over to the third row?
-- We can generate an infinite grid like this due to laziness

main :: IO ()
main = print $ iterate (scanl1 (+)) (repeat 1) !! 20 !! 20
