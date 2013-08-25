module Main where

toDigits :: Integer -> [Integer]

toDigits  0 = []
toDigits  l = (mod l 10) : toDigits  (div l 10)


toDigitsRev x = x

main::IO()
main = do
       putStr "toDigits "
       putStrLn . show $ toDigits 1234
       putStr "toDigitsRev "
       putStrLn . show $ toDigitsRev 1234