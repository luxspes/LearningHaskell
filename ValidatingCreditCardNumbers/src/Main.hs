module Main where

toDigits x = x
toDigitsRev x = x

main::IO()
main = do
       putStr "toDigits "
       putStrLn (show (toDigits 1234))
       putStr "toDigitsRev "
       putStrLn (show (toDigitsRev 1234))