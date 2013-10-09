module Main where


type PegName = String
type Move = (PegName, PegName)
type Peg = (PegName,[Integer])
hanoi :: Integer -> PegName -> PegName -> PegName -> [Move]
hanoi disks t1 t2 t3 =  getmoves [(t1,[1..disks]),(t2,[]),(t3,[])]

getmoves:: [Peg]->[Move]
getmoves pegs  = []

removeDisk::Peg->PegName->Peg
removeDisk (name,[]) _ = (name,[])
removeDisk (name,ds) source = if(name==source) then (name,(init ds)) else (name,ds)

addDisk::Peg->PegName->Peg->Peg
addDisk (name,ds) target (nametomove,dstomove) = if(name==target) then (name,ds++[(last dstomove)]) else (name,ds)

selectSource::[Peg]->Move->Peg
selectSource list move = (head (filter matchPeg list)) where matchPeg peg = fst(peg)==fst(move)
                                                                            
makeMove:: [Peg]->Move->[Peg]

makeMove pegs move = helper [] pegs move pegs where helper acc (peg:ps) (source,target) originalpegs = helper (acc++[(addDisk (removeDisk peg source) target (selectSource originalpegs (source,target) ) )]) ps (source,target) originalpegs 
                                                    helper acc _ _ _  = acc
                                                        


makeMoves:: [Peg]->[Move]->[Peg]
makeMoves pegs [] = pegs
makeMoves pegs [move] = makeMove pegs move
makeMoves pegs (m:ms) = makeMoves (makeMove pegs m) ms


main::IO()
main = do
  print [ ("t1",[1,2]),("t2",[]),("t3",[])]
  print $ makeMoves [("t1",[1,2]),("t2",[]),("t3",[])] [("t1","t2")];
  print $ makeMoves [("t1",[1,2]),("t2",[]),("t3",[])] [("t1","t2"),("t1","t3")];
  print $ makeMoves [("t1",[1,2]),("t2",[]),("t3",[])] [("t1","t2"),("t1","t3"),("t2","t3")];