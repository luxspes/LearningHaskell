module Main where


type PegName = String
type Move = (PegName, PegName)
type Transform = (PegName, PegName)
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

transformMove::Move->Transform->Move
transformMove (source, target) (source',target') = (if source==source' then target' else source,if target==source' then target' else target)

mergeMoves::Move->Move->Move->Move
mergeMoves (source,target) (source',target') (source'',target'') = (mergePegNames source source' source'' , mergePegNames target target' target'' )

mergePegNames::PegName->PegName->PegName->PegName
mergePegNames original change change' = if original==change then change' else change

makeTransformation::[Move]->Transform->[Move]
makeTransformation moves (source',target') = [ mergeMoves move (transformMove move (source',target')) (transformMove move (target',source')) | move <- moves] 

makeTransformations:: [Move]->[Transform]->[Move]
makeTransformations moves [] = moves
makeTransformations moves [transform] = makeTransformation moves transform
makeTransformations moves (t:ts) = makeTransformations (makeTransformation moves t) ts

moves:: Integer->[Move]
moves 1 = [("t1","t3")]
moves 2 = (makeTransformation (moves 1) ("t3","t2")) ++ (moves 1) ++ (makeTransformation (moves 1) ("t2","t1") )
moves 3 = (makeTransformation (moves 2) ("t3","t2")) ++ (moves 1) ++ (makeTransformation (moves 2) ("t2","t1") )
moves 4 = (makeTransformation (moves 3) ("t3","t2")) ++ (moves 1) ++ (makeTransformation (moves 3) ("t2","t1") )
moves 5 = (makeTransformation (moves 4) ("t3","t2")) ++ (moves 1) ++ (makeTransformation (moves 4) ("t2","t1") )

            
hanoi:: Integer->[Peg]->[Peg]
hanoi 1 pegs = makeMoves pegs (moves 1)
hanoi 2 pegs = makeMoves pegs (moves 2)
hanoi 3 pegs = makeMoves pegs (moves 3)
hanoi 4 pegs = makeMoves pegs (moves 4)
hanoi 5 pegs = makeMoves pegs (moves 5)

create3Pegs:: Integer->[Peg]
create3Pegs n  = [ ("t1",[1..n]),("t2",[]),("t3",[])]


main::IO()
main = do
  print "1" 
  print $ create3Pegs 1;
  print $ moves 1
  print $ hanoi 1 $ create3Pegs 1;
  print "2"     
  print $ create3Pegs 2;
  print $ moves 2
  print $ hanoi 2 $ create3Pegs 2;
  print "3"
  print $ create3Pegs 3;
  print $ moves 3
  print $ hanoi 3 $ create3Pegs 3;
  print "4" 
  print $ create3Pegs 4;
  print $ moves 4
  print $ hanoi 4 $ create3Pegs 4;
  print $ create3Pegs 5;
  print $ moves 5
  print $ hanoi 5 $ create3Pegs 5;
  