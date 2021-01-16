import Data.List

import Data.Maybe 

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
  = (fromJust .) . lookup

checkSat :: BDD -> Env -> Bool
checkSat (root, xs) env 
  | root == 1 = True  
  | root == 0 = False 
  | otherwise = checkSat (root', xs') env
  where
    (i, l, r) = lookUp root xs   
    bool      = lookUp i env
    root'     = if bool then r else l 
    xs'       = filter (\(a, _) -> a /= root) xs 

sat :: BDD -> [[(Index, Bool)]]
sat (root, xs)  
  | root == 1 = [[]]
  | root == 0 = [] 
  | otherwise = satLeft ++ satRight 
  where 
    (i, l, r) = lookUp root xs    
    satLeft   = map ((i, False) : ) (sat (l, xs)) 
    satRight  = map ((i, True) : ) (sat (r, xs)) 

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify exp  
  | Prim b <- exp 
    = Prim b 
  | Not (Prim b) <- exp
    = Prim $ not b
  | Not exp' <- exp 
    = Not (simplify exp') 
  | And (Prim b1) (Prim b2) <- exp  
    = Prim $ b1 && b2   
  | Or (Prim b1) (Prim b2) <- exp  
    = Prim $ b1 || b2 
  | And exp1 exp2 <- exp 
    = And (simplify exp1) (simplify exp2) 
  | Or exp1 exp2 <- exp 
    = Or (simplify exp1) (simplify exp2) 
  | IdRef x <- exp 
    = IdRef x     

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef x) n b 
  | x == n    = Prim b 
  | otherwise = IdRef x 
restrict (Not exp') n b 
  = simplify (Not (restrict exp' n b)) 
restrict (And exp1 exp2) n b 
  = simplify (And (restrict exp1 n b) (restrict exp2 n b))
restrict (Or exp1 exp2) n b 
  = simplify (Or (restrict exp1 n b) (restrict exp2 n b)) 
restrict (Prim x) _ _  
  = Prim x

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD exp 
  = buildBDD' exp 2

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim b) _ _  
  | b        = (1, [])
  |otherwise = (0, [])
buildBDD' (IdRef i) n _ 
  = (n, [(n, (i, 0, 1))])
buildBDD' exp n (x : xs)
  = (n, ls ++ rs ++ [(n, (x, l, r))])
  where 
    expTrue  = restrict exp x True   
    expFalse = restrict exp x False
    (r, rs)  = buildBDD' expTrue (2 * n + 1) xs
    (l, ls)  = buildBDD' expFalse (2 * n) xs 


------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD exp 
  = buildROBDD' exp 2

buildROBDD' :: BExp -> NodeId -> [Index] -> BDD
buildROBDD' (Prim b) _ _  
  | b        = (1, [])
  |otherwise = (0, [])
buildROBDD' (IdRef i) n _ 
  = (n, [(n, (i, 0, 1))])
buildROBDD' exp n (x : xs)
  | ln == rn  = buildROBDD' expTrue n xs 
  | otherwise = (n, ls ++ rs ++ [(n, (x, l, r))])
  where 
    expTrue  = restrict exp x True   
    expFalse = restrict exp x False
    (r, rs)  = buildROBDD' expTrue (2 * n + 1) xs
    (l, ls)  = buildROBDD' expFalse (2 * n) xs 
    (ln, rn) = (buildROBDD expFalse xs, buildROBDD expTrue xs)
       

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])

