module Main where


type Peg = String
type Move = (Peg, Peg)
type Pegs = ((Peg,[Integer]),(Peg,[Integer]),(Peg,[Integer]))
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disks t1 t2 t3 =  getmoves ((t1,[1..disks]),(t2,[]),(t3,[]))

getmoves:: Pegs->[Move]
getmoves pegs  = []

getmove:: Pegs->(Move,Pegs)
getmove pegs  = (("",""), (("",[]),("",[]),("",[])) )


main::IO()
main = do
  print $ hanoi 3 "t1" "t2" "t3"
