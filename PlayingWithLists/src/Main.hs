module Main where

list1 = [1,2,3,4]

main::IO()
main = do
        print list1
        print $  reverse list1 