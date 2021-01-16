data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix ns xs  
  = take (length ns) xs == ns 

removePrefix :: String -> String -> String
removePrefix ns 
--Pre: s is a prefix of s'
  = drop (length ns) 

suffixes :: [a] -> [[a]]
suffixes xs 
  = take (length xs) (iterate tail xs)

isSubstring :: String -> String -> Bool
isSubstring ns xs
  = any (isPrefix ns) (suffixes xs)

findSubstrings :: String -> String -> [Int]
findSubstrings ns xs 
  = [i | let ms = suffixes xs, i <- [0..length xs - 1], isPrefix ns (ms!!i)]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n)
  = [n]
getIndices (Node xs) 
  | [] <- xs      
    = [] 
  | (_, x) : xs' <- xs 
    = getIndices x ++ getIndices (Node xs')     

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition xs ms 
  = partition' [] xs ms 
  where 
    partition' ns [] ms 
      = (ns, [], ms)
    partition' ns xs [] 
      = (ns, xs, [])
    partition' ns a@(x : xs) b@(m : ms) 
      | x == m    = partition' (ns ++ [m]) xs ms 
      | otherwise = (ns, a, b)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf n) 
  | "" <- s   = [n]
  | otherwise = [] 
findSubstrings' _ (Node []) 
  = [] 
findSubstrings' s (Node ((a, tree) : ms))
  | prefix == s = findSubstrings' "" tree ++ xs   
  | prefix == a = findSubstrings' s' tree ++ xs 
  | otherwise   = xs
  where 
    (prefix, _, s') = partition a s
    xs              = findSubstrings' s (Node ms)

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Leaf m)
  = Node [(s, Leaf n), ("", Leaf m)]  
insert (s, n) (Node [])
  = Node [(s, Leaf n)]
insert (s, n) (Node (x@(a, tree) : xs))
  | prefix == "" = Node (x : xs') 
  | prefix == a  = Node ((a, tree') : xs)
  | otherwise    = Node ((prefix, tree'') : xs)
  where 
    (prefix, a', s') = partition a s   
    Node xs'         = insert (s, n) (Node xs)
    tree'            = insert (s', n) tree 
    tree''           = Node [(a', tree), (s', Leaf n)]       

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

subStrings :: String -> [String]
subStrings xs 
  = [take n ms | ms <- suffixes xs, n <- [0..length ms]]

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

