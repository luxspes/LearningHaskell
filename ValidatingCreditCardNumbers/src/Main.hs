module Main where

toDigitsRev :: Integer -> [Integer]
-- como hacer esto con unfoldr?
toDigitsRev  0 = []
toDigitsRev  l = (mod (abs l) 10) : toDigitsRev  (div (abs l) 10)


toDigits = reverse . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev l = last l : reallyDoubleEveryOther  (init l)
                             where reallyDoubleEveryOther l = (last l) * 2 : doubleEveryOtherRev  (init l)

doubleEveryOther = reverse . doubleEveryOtherRev

main::IO()
main = do
       putStr "toDigits "
       putStrLn . show $ toDigits 1234
       putStr "toDigitsRev "
       putStrLn . show $ toDigitsRev 1234
       putStr "doubleEveryOther "
       putStrLn . show $ doubleEveryOther $ toDigits 1234