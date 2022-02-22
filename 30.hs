#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

-- just wait for this to run and found that only 6 numbers matched this. Wonder if there is a reason.

main :: IO ()
main = print . sum . take 6 $ filter (\n -> powerSum 5 n == n) [2 ..]

powerSum :: Int -> Int -> Int
powerSum p n = sum $ map ((^ p) . read . pure) (show n)