module Main where


type PegName = String
type Move = (PegName, PegName)
type Peg = (PegName,[Integer])
hanoi :: Integer -> PegName -> PegName -> PegName -> [Move]
hanoi disks t1 t2 t3 =  getmoves [(t1,[1..disks]),(t2,[]),(t3,[])]

getmoves:: [Peg]->[Move]
getmoves pegs  = []


selectSource::[Peg]->Move->[Peg]
selectSource list move = filter matchPeg list where matchPeg peg = fst(peg)==fst(move)

selectTarget::[Peg]->Move->[Peg]
selectTarget list move = filter matchPeg list where matchPeg peg = fst(peg)==snd(move)

selectOthers::[Peg]->Move->[Peg]
selectOthers list move = filter matchPeg list where matchPeg peg = not (fst(peg)==fst(move) || fst(peg)==snd(move))

makeMove:: [Peg]->Move->[Peg]
makeMove pegs move = (selectSource pegs move)++(selectTarget pegs move)++(selectOthers pegs move)

removeTop:: [Peg] -> [Peg]
removeTop [peg] =(fst(peg),remainingDisks):[] where remainingDisks = (init  originalDisks) where originalDisks = (snd peg)


main::IO()
main = do
  print [ ("t1",[1,2]),("t2",[]),("t3",[])]
  print $ makeMove [("t1",[1,2]),("t2",[]),("t3",[])] ("t1","t2");
