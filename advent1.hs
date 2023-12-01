import Data.Char (isDigit)
import Data.List (tails, inits)

--helper functions
getFirstAndLast :: String -> String
getFirstAndLast [x] = [x] ++ [x]
getFirstAndLast xs = [head xs] ++ [last xs]

toNum :: String -> String --lol
toNum "zero" = "0"
toNum "one" = "1"
toNum "two" = "2"
toNum "three" = "3"
toNum "four" = "4"
toNum "five" =  "5"
toNum "six" = "6"
toNum "seven" = "7"
toNum "eight" = "8"
toNum "nine" = "9"
toNum other = other

calibrate :: String -> Int --part one 
calibrate = read . getFirstAndLast . filter isDigit

calibrate2 :: String -> Int --part two
calibrate2 = read . getFirstAndLast . concatMap toNum . filter (\x -> elem x nums || length x == 1 && isDigit (head x)) . concatMap inits . tails 
             where nums = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

getSum :: (String -> Int) -> [String] -> Int 
getSum cal = sum . map cal