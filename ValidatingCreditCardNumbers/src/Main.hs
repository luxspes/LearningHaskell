module Main where


-- como hacer esto con unfoldr?
toDigitsRev  0 = []
toDigitsRev  l = (mod (abs l) 10) : toDigitsRev  (div (abs l) 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev l = last l : reallyDoubleEveryOther  (init l)
                             where reallyDoubleEveryOther l = (last l) * 2 : doubleEveryOtherRev  (init l)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev

sumDigits:: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = digitSum x + sumDigits xs 
                   where digitSum =  sum . toDigits 

validate:: Integer -> Bool
validate v = False



main::IO()
main = do
       putStr "toDigits "
       print . show $ toDigits 1234
       putStr "toDigitsRev "
       print . show $ toDigitsRev 1234
       putStr "doubleEveryOther "
       print . show $ doubleEveryOther $ toDigits 1234
       putStr "sumDigits "
       print . show $ sumDigits $ doubleEveryOther $ toDigits 8765
       putStr "validate "
       print . show $ validate 4012888888881881
       putStr "validate "
       print . show $ validate 4012888888881882
       