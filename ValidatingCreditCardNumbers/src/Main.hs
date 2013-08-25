module Main where

toDigits :: Integer -> [Integer]

toDigits x = (mod x 10) : toDigitList  (div x 10)
             where toDigitList  0 = []
                   toDigitList  l = (mod l 10) : toDigitList  (div l 10)


toDigitsRev x = x

main::IO()
main = do
       putStr "toDigits "
       putStrLn . show $ toDigits 1234
       putStr "toDigitsRev "
       putStrLn . show $ toDigitsRev 1234