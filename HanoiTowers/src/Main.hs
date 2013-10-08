module Main where


type PegName = String
type Move = (PegName, PegName)
data Pegs = Pegs { p1::(PegName,[Integer]), p2::(PegName,[Integer]) ,p3::(PegName,[Integer]) } deriving (Show)
hanoi :: Integer -> PegName -> PegName -> PegName -> [Move]
hanoi disks t1 t2 t3 =  getmoves Pegs {p1 = (t1,[1..disks]), p2 = (t2,[]), p3 =  (t3,[])}

getmoves:: Pegs->[Move]
getmoves pegs  = []

makeMove:: Pegs->Move->Pegs
makeMove pegs move = pegs


main::IO()
main = do
  print (Pegs ("t1",[1,2]) ("t2",[]) ("t3",[]))
  print $ makeMove (Pegs ("t1",[1,2]) ("t2",[]) ("t3",[])) ("t1","t2");
