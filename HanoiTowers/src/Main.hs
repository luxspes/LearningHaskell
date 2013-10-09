module Main where


type PegName = String
type Move = (PegName, PegName)
type Peg = (PegName,[Integer])

removeDisk::Peg->PegName->Peg
removeDisk (name,[]) _ = (name,[])
removeDisk (name,ds) source = if(name==source) then (name,(tail ds)) else (name,ds)

addDisk::Peg->PegName->Peg->Peg
addDisk (name,ds) target (nametomove,dstomove) = if(name==target) then (name,[(head dstomove)]++ds) else (name,ds)

selectSource::[Peg]->Move->Peg
selectSource list move = (head (filter matchPeg list)) where matchPeg peg = fst(peg)==fst(move)
                                                                            
makeMove:: [Peg]->Move->[Peg]

makeMove pegs move = let tomove = selectSource pegs move
                     in helper [] pegs move tomove where helper acc (peg:ps) (source,target) tomove = helper (acc++[(addDisk (removeDisk peg source) target tomove )]) ps (source,target) tomove 
                                                         helper acc _ _ _  = acc
                                                        


makeMoves:: [Peg]->[Move]->[Peg]
makeMoves pegs [] = pegs
makeMoves pegs [move] = makeMove pegs move
makeMoves pegs (m:ms) = makeMoves (makeMove pegs m) ms


hanoi:: Integer->[Peg]->[Peg]
hanoi 1 pegs = makeMoves pegs [("t1","t3")]
hanoi 2 pegs = makeMoves pegs             [("t1","t2"),             ("t1","t3"),            ("t2","t3")]
hanoi 3 pegs = makeMoves pegs [("t1","t3"),("t1","t2"),("t3","t2"), ("t1","t3"),("t2","t1"),("t2","t3"),("t1","t3")]


main::IO()
main = do
  print [ ("t1",[1]),("t2",[]),("t3",[])];
  print $ hanoi 1 [ ("t1",[1]),("t2",[]),("t3",[])];

  print [ ("t1",[1,2]),("t2",[]),("t3",[])];
  print $ hanoi 2 [ ("t1",[1,2]),("t2",[]),("t3",[])];
  
  print [ ("t1",[1,2,3]),("t2",[]),("t3",[])];
  print $ hanoi 3 [ ("t1",[1,2,3]),("t2",[]),("t3",[])];
  