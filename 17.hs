#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = print . sum $ map (length . numToEnglish) [1 .. 1000]

-- main = print $ map numToEnglish [1 .. 1000]

numToEnglish :: Int -> String
numToEnglish 1 = "one"
numToEnglish 2 = "two"
numToEnglish 3 = "three"
numToEnglish 4 = "four"
numToEnglish 5 = "five"
numToEnglish 6 = "six"
numToEnglish 7 = "seven"
numToEnglish 8 = "eight"
numToEnglish 9 = "nine"
numToEnglish 10 = "ten"
numToEnglish 11 = "eleven"
numToEnglish 12 = "twelve"
numToEnglish 13 = "thirteen"
numToEnglish 14 = "fourteen"
numToEnglish 15 = "fifteen"
numToEnglish 16 = "sixteen"
numToEnglish 17 = "seventeen"
numToEnglish 18 = "eighteen"
numToEnglish 19 = "nineteen"
numToEnglish 20 = "twenty"
numToEnglish 30 = "thirty"
numToEnglish 40 = "forty"
numToEnglish 50 = "fifty"
numToEnglish 60 = "sixty"
numToEnglish 70 = "seventy"
numToEnglish 80 = "eighty"
numToEnglish 90 = "ninety"
numToEnglish 1000 = numToEnglish 1 ++ "thousand"
numToEnglish n
  | n < 100 = numToEnglish (n `div` 10 * 10) ++ numToEnglish (n `rem` 10)
  | n `rem` 100 == 0 = hundreds
  | otherwise = hundreds ++ "and" ++ numToEnglish (n `rem` 100)
  where
    hundreds = numToEnglish (n `div` 100) ++ "hundred"
