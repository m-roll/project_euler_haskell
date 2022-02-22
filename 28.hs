#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print $ prob28

prob28 :: Int
prob28 = sum (take (1001 * 2 - 1) intSpiral)

intSpiral :: [Int]
intSpiral = f [1 ..] skips
  where
    f :: [Int] -> [Int] -> [Int]
    f (n : ns) (skip_next : skips) = n : f (drop skip_next ns) skips

skips = concatMap (replicate 4) [1, 3 ..]