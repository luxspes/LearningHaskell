module Main where


type PegName = String
type Move = (PegName, PegName)
type Peg = (PegName,[Integer])
hanoi :: Integer -> PegName -> PegName -> PegName -> [Move]
hanoi disks t1 t2 t3 =  getmoves [(t1,[1..disks]),(t2,[]),(t3,[])]

getmoves:: [Peg]->[Move]
getmoves pegs  = []

{--selectSource::[Peg]->Move->[Peg]
selectSource list move = filter matchPeg list where matchPeg peg = fst(peg)==fst(move)

selectTarget::[Peg]->Move->[Peg]
selectTarget list move = filter matchPeg list where matchPeg peg = fst(peg)==snd(move)

selectOthers::[Peg]->Move->[Peg]
selectOthers list move = filter matchPeg list where matchPeg peg = not (fst(peg)==fst(move) || fst(peg)==snd(move))

removeTop:: [Peg] -> [Peg]
removeTop [peg] =(fst(peg),remainingDisks):[] where remainingDisks = (init  originalDisks) where originalDisks = (snd peg)

getTop:: [Peg] -> Integer
getTop [peg] = (last originalDisks) where originalDisks = (snd peg)

addTop:: [Peg]->[Peg]-> [Peg]
addTop [targetPeg] sourcePeg =(fst(targetPeg),newDisks):[] where newDisks = originalDisks++[getTop sourcePeg] 
                                                                        where originalDisks = (snd targetPeg)
--}

removeDisk::Peg->PegName->Peg
removeDisk (name,[]) _ = (name,[])
removeDisk (name,ds) source = if(name==source) then (name,(init ds)) else (name,ds)

getTop::Peg->PegName->[Integer]
getTop (name,[]) _ = []
getTop (name,ds) source = if(name==source) then [(last ds)] else []

addDisk::Peg->PegName->[Integer]->Peg
addDisk (name,ds) target diskToAdd = if(name==target) then (name,ds++diskToAdd) else (name,ds)
                                                                            
makeMove:: [Peg]->Move->[Peg]
--makeMove pegs move = removeTop (selectSource pegs move)++addTop (selectTarget pegs move) (selectSource pegs move) ++(selectOthers pegs move)
makeMove pegs move = helper ([],[]) pegs move where helper (accl,acct) (peg:ps) (source,target) = helper ([(addDisk (removeDisk peg source) target acct)]++accl,(getTop peg source)++acct) ps (source,target) 
                                                    helper (accl,acct) _ _ = accl


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