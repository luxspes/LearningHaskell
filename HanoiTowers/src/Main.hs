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

transform::Peg->PegName->Peg
transform (n,[]) _ = (n,[])
transform (n,d) s = if(n==s) then (n,(init d)) else (n,d)

getTop::Peg->PegName->[Integer]
getTop (n,[]) _ = []
getTop (n,d) t = if(n==t) then [(last d)] else []
                                                                            
makeMove:: [Peg]->Move->[Peg]
--makeMove pegs move = removeTop (selectSource pegs move)++addTop (selectTarget pegs move) (selectSource pegs move) ++(selectOthers pegs move)
makeMove pegs move = helper ([],[]) pegs move where helper (accl,acct) (peg:ps) (source,target) = helper ([(transform peg source)]++accl,[(getTop peg target)]++acct) ps (source,target) 
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