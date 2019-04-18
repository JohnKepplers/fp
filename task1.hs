
data Node v = Nil | Node {value :: Maybe v, childs :: [(Char, Node v)]} deriving (Eq, Show)

newtype Trie v = Trie {root :: Node v} deriving (Eq, Show)

type Dictionary = Trie String 

get :: String -> Dictionary -> Maybe String
get "" _ = Just ""
get s d = get2 node (childs node) s
              where node = root d


get2 :: Node String -> [(Char, Node String)]-> String -> Maybe String
get2 n ((x, y):xs)(c:cs) | c == x && (cs == []) = value y 
						             | c == x               = get2 y (childs y) cs
                         | xs == []             = Nothing
					               |otherwise             = get2 n xs (c:cs)
						 
						 
remove :: String -> Dictionary -> (Maybe String, Dictionary)
remove s d | v == Nothing  = (v, d)
		       | otherwise     = (v, Trie (remove2 s (root d)))
		           where v = get s d
			   
remove2 :: String -> Node String -> Node String
remove2 "" node  = Node Nothing (childs node)
remove2(c:cs) node = Node (value node) (map remove3 (childs node)) 
                        where remove3 (key, child) | key == c  = (key, remove2 cs child)
						                                       | otherwise = (key, child)
						 
