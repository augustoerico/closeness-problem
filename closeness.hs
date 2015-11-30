import Data.List

-- Get all paths
paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths source target graph   = paths1 source target graph []
 
paths1 :: Eq a => a -> a -> [(a,a)] -> [a] -> [[a]]
paths1 source target graph current  = paths2 source target graph current ([snd n | n <- graph, fst n == source] ++ [fst n | n <- graph, snd n == source ])

paths2 :: Eq a => a -> a -> [(a,a)] -> [a] -> [a] -> [[a]]
paths2 source target graph current []
    | source == target  = [current ++ [source]]
	| otherwise         = []
paths2 source target graph current (x:xs) 
    | source == target      = [current ++ [source]]
    | elem source current   = []
    | otherwise             = 
        (paths1 x target graph (current ++ [source])) ++ (paths2 source target graph current xs)

-- Get the size of the shortest list
shortest (x:xs) = shortest' xs (length x)
shortest' [] current    = current
shortest' (x:xs) current
    | current > length x = shortest' xs (length x)
    | otherwise                 = shortest' xs current
    
-- Get all nodes from graph
nodes graph = nodes' graph [] 
nodes' [] _     = []
nodes' (x:xs) n 
    | fst x /= snd x    = first x n ++ second x n ++ nodes' xs (n ++ first x n ++ second x n)
    | otherwise         = first x n ++ nodes' xs (n ++ first x n)
first x n
    | fst x `elem` n    = []
    | otherwise         = [fst x]
second x n
    | snd x `elem` n    = []
    | otherwise         = [snd x]

farness node graph
    = sum [shortest (paths node target graph) - 1 | target <- (nodes graph)]
--    = [(target, shortest (paths node target graph)) | target <- (nodes graph)]
