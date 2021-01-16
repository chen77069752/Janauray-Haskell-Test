module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes n 
  | n <= 1 
    = n 
  | otherwise  
    = countOnes (n `div` 2) + n `mod` 2 

countOnes' :: Int -> Int 
countOnes' n   
  | n <= 15   = bitTable !! n 
  | otherwise = bitTable !! (n `mod` 8) + countOnes' (n `div` 8)

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n 
  = countOnes ((bit i - 1) .&. n)  

getIndex :: Int -> Int -> Int -> Int
getIndex n i size  
  = shiftR n (i * size) .&. (bit size - 1) 

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace i xs v 
  = take i xs ++ [v] ++ drop (i + 1) xs   

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt i v xs 
  = take i xs ++ [v] ++ drop i xs 

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ f2 (Leaf xs)
  = f2 xs 
sumTrie f1 f2 (Node _ sn) 
  = sum $ map (sumNode f1 f2) sn 
  where 
    sumNode f1 _ (Term n) 
      = f1 n 
    sumNode f1 f2 (SubTrie trie) 
      = sumTrie f1 f2 trie     
         
--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member n h trie b 
  = member' trie 0 
  where 
    member' (Leaf xs) _ 
      = n `elem` xs
    member' (Node v xs) lv 
      | testBit v index 
        = case xs !! pos of 
            Term x -> x == n 
            SubTrie trie' -> member' trie' (lv + 1)
      | otherwise 
        = False        
      where 
        index = getIndex h lv b     
        pos   = countOnesFrom index v 

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf depth size v trie 
  = insert' trie v 0
  where 
    insert' (Leaf xs) v _ 
      = Leaf (v : filter (/=v) xs)
    insert' _ v lv 
      | lv == depth - 1 = Leaf [v] 
    insert' (Node vector sn) v lv 
      | testBit vector index
        = Node vector (replace pos sn subNode)  
      | otherwise 
        = Node (setBit vector index) (insertAt pos (Term v) sn)    
      where 
        index = getIndex (hf v) lv size   
        pos   = countOnesFrom index vector 
        subNode :: SubNode
        subNode = case sn !! pos of    
                    SubTrie trie' -> SubTrie (insert' trie' (hf v) (lv + 1)) 
                    Term v'  
                      | v == v' -> Term v
                      | otherwise -> SubTrie (insert' (insert' empty v' (lv + 1)) v (lv + 1))       

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf depth size 
  = foldr (insert hf depth size) empty  