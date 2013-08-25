module Main where

toDigits :: Integer -> [Integer]

toDigits x = (mod x 10) : toDigitList (mod x 10) (div x 10)
             where toDigitList d 0 = []
                   toDigitList d l = (mod l 10) : toDigitList (mod l 10) (div l 10)


toDigitsRev x = x

main::IO()
main = do
       putStr "toDigits "
       putStrLn . show $ toDigits 1234
       putStr "toDigitsRev "
       putStrLn . show $ toDigitsRev 1234