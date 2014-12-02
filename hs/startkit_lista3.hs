--computes real roots from a 2nd order equation
quadsolve :: Float -> Float -> Float -> [Float]

--True if the list contains no duplicate elements
pertence  :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (y:ys) = x==y || pertence x ys

isSet :: Eq a => [a] -> Bool
isSet [] = True
isSet (x:xs) = not (pertence x xs) && (isSet xs) 

--Remove duplicate elements from a list.
makeSet :: Eq a => [a] -> [a]

--True if every element in the first set also occurs in the second set.
subset :: Eq a => [a] -> [a] -> Bool

--True if the two sets contain the same elements (not necessarily in the same order).
equalSet :: Eq a => [a] -> [a] -> Bool

--The set of values that are in the first set but not in the second set.
setDiff :: Eq a => [a] -> [a] -> [a]

--The set of values that are in either or both sets.
setUnion :: Eq a => [a] -> [a] -> [a]

--The set of values that are in both sets.
setIntersection :: Eq a => [a] -> [a] -> [a]

--lists all possible permutaions of a given list
perms :: [a] -> [[a]]
