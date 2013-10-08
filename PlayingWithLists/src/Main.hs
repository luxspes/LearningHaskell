module Main where

list1 = [1,2,3,4]

main::IO()
main = do
        print list1
        print $  reverse list1 
        print $ filter odd list1 
        print $ filter (\x->x==4) list1