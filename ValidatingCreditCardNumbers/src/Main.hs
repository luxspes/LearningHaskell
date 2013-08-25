module Main where

toDigitsRev :: Integer -> [Integer]

toDigitsRev  0 = []
toDigitsRev  l = (mod (abs l) 10) : toDigitsRev  (div (abs l) 10)


toDigits = reverse . toDigitsRev

main::IO()
main = do
       putStr "toDigits "
       putStrLn . show $ toDigits 1234
       putStr "toDigitsRev "
       putStrLn . show $ toDigitsRev 1234