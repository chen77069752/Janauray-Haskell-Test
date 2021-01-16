import Data.Maybe
import Data.List
       hiding (insert) 
type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node a _ _)  
  = a

rank :: BinTree a -> Int
rank (Node _ n _)
  = n 

children :: BinTree a -> [BinTree a]
children (Node _ _ xs)
  = xs 

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1 t2 
  | v1 < v2   = Node v1 (r + 1) (t2 : children t1) 
  | otherwise = Node v2 (r + 1) (t1 : children t2)
  where 
    r      = rank t1   
    v1     = value t1   
    v2     = value t2 


--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin 
  = minimum . map value

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h1 []
  = h1 
mergeHeaps [] h2 
  = h2 
mergeHeaps h1@(t1 : h1') h2@(t2 : h2')
  | rank t1 < rank t2 
    = t1 : mergeHeaps h1' h2 
  | rank t2 < rank t1 
    = t2 : mergeHeaps h1 h2' 
  | otherwise
    = mergeHeaps [combineTrees t1 t2] (mergeHeaps h1' h2')      

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert n 
  = mergeHeaps [Node n 0 []] 

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin t
  = mergeHeaps (reverse (children n)) t' 
  where 
    (n, t') = removeMin t    

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove m t 
  = t' 
  where 
    n  = fromJust $ find (\x -> value x == m ) t 
    t' = delete n t   

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin t 
  = (n, t')
  where 
    n  = fromJust $ find (\x -> value x == m ) t  
    m  = extractMin t 
    t' = remove m t  

binSort :: Ord a => [a] -> [a]
binSort xs 
  = map extractMin (take (length xs) ms) 
  where 
    ls   = map (\x -> insert x []) xs  
    bMap = foldl1 mergeHeaps ls 
    ms   = iterate deleteMin bMap

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary [] 
  = [] 
toBinary xs
  = toBinary' (map rank xs) 0
  where 
    toBinary' [] _ 
      = [] 
    toBinary' l@(r : rs) n 
      | r == n    = toBinary' rs (n + 1) ++ [1]
      | otherwise = toBinary' l (n + 1) ++ [0]    


binarySum :: [Int] -> [Int] -> [Int]
binarySum xs ms 
  = binarySum' xs ms 0 
  where 
    binarySum' [] ms c  
      | c == 0 = ms 
      | c == 1 = binarySum [c] ms 
    binarySum' xs [] c 
      | c == 0 = xs
      | c == 1 = binarySum [c] xs     
    binarySum' xs ms c 
      | sum == 1  = binarySum' xs' ms' 0 ++ [1]
      | sum == 0  = binarySum' xs' ms' 0 ++ [0]
      | otherwise = binarySum' xs' ms' 1 ++ [0] 
      where 
        x   = last xs
        xs' = init xs  
        m   = last ms 
        ms' = init ms 
        sum = x + m + c  

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]


