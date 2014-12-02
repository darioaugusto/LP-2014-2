--converts from celsius to farenheit
celsiusToFarenheit :: Fractional a => a -> a
celsiusToFarenheit x = x * 1.8 + 32

--computes real roots from a 2nd order equation
quadsolve a b c
       | delta < 0  = error "complex roots"
       | delta == 0 = [-b/(2*a)]
       | delta > 0  = [-b/(2*a) + radix/(2*a),
                             -b/(2*a) - radix/(2*a)]
       where
           delta = b*b - 4*a*c
           radix = sqrt delta

--True if the list contains no duplicate elements
pertence  :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (y:ys) = x==y || pertence x ys

isSet :: Eq a => [a] -> Bool
isSet [] = True
isSet (x:xs) = not (pertence x xs) && (isSet xs) 

--Remove duplicate elements from a list.
makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs) | pertence x xs = makeSet xs
			   | otherwise = [x] ++ makeSet xs

--True if every element in the first set also occurs in the second set.
subset :: Eq a => [a] -> [a] -> Bool
subset [] [] = True
subset [] _ = True
subset (x:xs) ys = (pertence x ys) && subset xs ys

--True if the two sets contain the same elements (not necessarily in the same order).
equalSet :: Eq a => [a] -> [a] -> Bool
equalSet [] [] = True
equalSet xs ys = (subset xs ys) && (subset ys xs)

--The set of values that are in the first set but not in the second set.
setDiff :: Eq a => [a] -> [a] -> [a]
setDiff [] [] = []
setDiff xs [] = xs
setDiff [] ys = []
setDiff (x:xs) ys | pertence x ys = setDiff xs ys
			   	  | otherwise = [x] ++ setDiff xs ys

--The set of values that are in either or both sets.
setUnion :: Eq a => [a] -> [a] -> [a]
setUnion xs ys = setDiff xs ys ++ makeSet ys

--The set of values that are in both sets.
setIntersection :: Eq a => [a] -> [a] -> [a]
setIntersection [] [] = []
setIntersection xs [] = []
setIntersection [] ys = []
setIntersection (x:xs) ys | pertence x ys = [x] ++ setIntersection xs ys
						  | otherwise = setIntersection xs ys

--lists all possible permutaions of a given list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (h:t) =
    [take n x ++ [h] ++ drop n x
        | n <- [0..length t], x <- perms t]
        