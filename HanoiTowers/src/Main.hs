module Main where


type Peg = String
type Move = (Peg, Peg)
data Pegs = Pegs { p1::(Peg,[Integer]), p2::(Peg,[Integer]) ,p3::(Peg,[Integer]) } deriving (Show)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disks t1 t2 t3 =  getmoves Pegs {p1 = (t1,[1..disks]), p2 = (t2,[]), p3 =  (t3,[])}

getmoves:: Pegs->[Move]
getmoves pegs  = []

getmove:: Pegs->(Move,Pegs)
--vacio
getmove (Pegs (t1,[]) (t2,[]) (t3,[])) = (("",""), Pegs (t1,[]) (t2,[]) (t3,[]) )
-- uno
getmove (Pegs (t1,[a]) (t2,[]) (t3,[])) = ((t1,t3), Pegs (t1,[]) (t2,[]) (t3,[a]) )
-- dos 
getmove (Pegs (t1,[a,b]) (t2,[]) (t3,[])) = ((t1,t2), Pegs (t1,[b]) (t2,[a]) (t3,[]) )
getmove (Pegs (t1,[b]) (t2,[a]) (t3,[])) = ((t1,t3), Pegs (t1,[]) (t2,[a]) (t3,[b]) )
getmove (Pegs (t1,[]) (t2,[a]) (t3,[b])) = ((t2,t3), Pegs (t1,[]) (t2,[]) (t3,[a,b]) )
--getmove pegs                         = (("",""), Pegs ("",[]) ("",[]) ("",[]) )


main::IO()
main = do
  print $ hanoi 2 "t1" "t2" "t3"
