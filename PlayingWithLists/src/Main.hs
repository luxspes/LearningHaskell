module Main where

listaSimple = [1,2,3,4]
listaDeTuplas = [("a",1),("b",2),("c",3),("d",4)]

main::IO()
main = do
        print listaSimple
        print $ reverse listaSimple 
        print $ filter odd listaSimple 
        print $ filter (\x->x==4) listaSimple
        print listaDeTuplas