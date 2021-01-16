module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp 
  = (fromJust .) . lookup

-- 3 marks
vars :: Formula -> [Id]
vars (Var a)
  = [a]
vars (Not f)
  = (sort . nub) $ vars f 
vars (And f1 f2)
  = (sort . nub) $ vars f1 ++ vars f2
vars (Or f1 f2)
  = (sort . nub) $ vars f1 ++ vars f2     

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (And f1 f2))
  = Or (toNNF $ Not f1) (toNNF $ Not f2)
toNNF (Not (Or f1 f2))
  = And (toNNF $ Not f1) (toNNF $ Not f2)
toNNF (Not (Not f))
  = toNNF f 
toNNF (And f1 f2) 
  = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2)
  = Or (toNNF f1) (toNNF f2)
toNNF f 
  = f    

-- 3 marks
toCNF :: Formula -> CNF
toCNF f
  = toCNF' nnf 
  where 
    nnf = toNNF f 
    toCNF' (Or f1 f2)
      = distribute f1 f2 
    toCNF' (And f1 f2)
      = And (toCNF' f1) (toCNF' f2)
    toCNF' f
      = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten cnf 
  = flatten' cnf xs
  where 
    xs = idMap cnf

flatten' :: Formula -> [(Id, Int)] -> [[Int]]
flatten' (Not (Var x)) xs 
  = [[- (lookUp x xs)]]
flatten' (Var x) xs 
  = [[lookUp x xs]]
flatten' (Or f1 f2) xs 
  = [head (flatten' f1 xs) ++ head (flatten' f2 xs)]
flatten' (And f1 f2) xs 
  = flatten' f1 xs ++ flatten' f2 xs   

--------------------------------------------------------------------------
-- Part III

-- 5 marks
singleton :: [[a]] -> [a]
singleton [] = [] 
singleton (x@[_] : _) = x 
singleton (_ : xs) = singleton xs  

sub :: CNFRep -> [Int] -> CNFRep 
sub [] _ 
  = [] 
sub (x : xs) [s] 
  | s `elem` x 
    = sub xs [s]
  | (-s) `elem` x 
    = delete (-s) x : sub xs [s] 
  | otherwise 
    = x : sub xs [s]        

propUnits :: CNFRep -> (CNFRep, [Int])
propUnits xs   
  | null s 
    = (xs, []) 
  | otherwise 
    = (xs'', s ++ ns)     
    where 
      s          = singleton xs
      xs'        = delete s xs  
      (xs'', ns) = propUnits (sub xs' s)

-- 4 marks
dp :: CNFRep -> [[Int]]
dp xs 
  = filter (not . null) $ dp' xs [] 
  where 
    dp' :: CNFRep -> [Int] -> [[Int]]
    dp' zs ms 
      | null zs'
        = [ms ++ ss] 
      | any null zs' 
        = []  
      | otherwise 
        = dp' ([firstEle] : zs') (ss ++ ms) 
          ++ dp' ([- firstEle] : zs') (ss ++ ms)      
        where 
          (zs', ss) = propUnits zs 
          firstEle  = head $ head zs'     

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f 
  = map (`allSat'` iMap) ls
  where 
    ls   = dp $ flatten (toCNF f )
    iMap = map (\(a, b) -> (b, a)) $ idMap f

allSat' :: [Int] -> [(Int, Id)] -> [(Id, Bool)]
allSat' ns table 
  = map (\n -> (lookUp (abs n) table, n > 0)) ns
  

