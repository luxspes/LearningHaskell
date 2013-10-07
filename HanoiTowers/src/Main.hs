module Main where


type Peg = String
type Move = (Peg, Peg)
data Pegs = Pegs { p1::(Peg,[Integer]), p2::(Peg,[Integer]) ,p3::(Peg,[Integer]) } deriving (Show)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disks t1 t2 t3 =  getmoves Pegs {p1 = (t1,[1..disks]), p2 = (t2,[]), p3 =  (t3,[])}

getmoves:: Pegs->[Move]
getmoves pegs  = []

getmove:: Pegs->(Move,Pegs)
getmove pegs  = (("",""), Pegs ("",[]) ("",[]) ("",[]) )


main::IO()
main = do
  print $ hanoi 3 "t1" "t2" "t3"
