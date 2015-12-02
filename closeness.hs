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
shortest :: Eq a => [[a]] -> Int
shortest (x:xs) = shortest' xs (length x)
shortest' :: Eq a => [[a]] -> Int -> Int
shortest' [] current        = current
shortest' (x:xs) current
    | current > length x    = shortest' xs (length x)
    | otherwise             = shortest' xs current
    
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

farness :: Eq a => a -> [(a,a)] -> Int
farness node graph
    = sum [shortest (paths node target graph) - 1 | target <- (nodes graph)]

-- FIXME this always returns 0...
closeness' :: Eq a => a -> [(a,a)] -> Float
closeness' node graph
    = 1.0 / (fromIntegral (farness node graph))
    
-- Return a list of tuples (node, closeness)
-- TODO create a list already sorted
closeness graph = 
    [(n, (closeness' n graph)) | n <- (nodes graph)]
    
-- Sort (node, closeness) tuple using quicksort
rankCloseness []    = []
rankCloseness (x:xs) =
    rankCloseness [a | a <- xs, snd a > snd x ] ++ [x] ++ rankCloseness [a | a <- xs, snd a <= snd x]

{- ----------------------------------------------------------------------------
Test cases - TODO put this in a different file
---------------------------------------------------------------------------- -}
testFarnessGraph1 n r
    | result == r   = True  -- success
    | otherwise     = False -- failure
    where result = farness n [(1,2),(1,3),(1,4),(5,2),(6,2),(3,6),(6,7),(7,3)]
    
-- FIXME which one failed?
testFarness1 = testFarnessGraph1 1 9
testFarness2 = testFarnessGraph1 3 10
testFarness3 = testFarnessGraph1 7 12
{- 
testFarness 
    | testFarness1 and testFarness2 and testFarness3 = "Test farness OK"
    | otherwise = "Test farness NOT OK"
-}

-- FIXME this test should consider some way to untie
testRankCloseness input r
    | result == r   = True
    | otherwise     = False
    where result = rankCloseness input
    
testRankCloseness1 = testRankCloseness [(1,1/1234),(2,1/987),(3,1/800),(4,1/732),(5,1/667),(6,1/500)] [(6,1/500),(5,1/667),(4,1/732),(3,1/800),(2,1/987),(1,1/1234)]
testRankCloseness2 = testRankCloseness [(1,1/2),(2,2/5),(3,3/7)] [(1,1/2),(3,3/7),(2,2/5)]
testRankCloseness3 = testRankCloseness [(5,0.07142857),(6,0.1),(1,0.1111111),(2,0.1111111),(3,0.1),(7,0.08333334),(4,0.07142857)] [(1,0.1111111),(2,0.1111111),(6,0.1),(3,0.1),(7,0.08333334),(5,0.07142857),(4,0.07142857)]


{- BROKEN...
testCloseness
    | result == div 1 9 = "Test closeness OK!"
    | otherwise     = "Test closeness FAILED!"
    where result = closeness 1 [(1,2),(1,3),(1,4),(5,2),(6,2),(3,6),(6,7),(7,3)]
    -}