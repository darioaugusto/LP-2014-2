-- Gabarito Dario
-- 1
-- a) OK
crosswordsFindLC :: Char -> Int -> Int -> [String] -> [String]
crosswordsFindLC ch pos tam xs = [x | x <- xs, (length x) == tam, x!!pos == ch]

-- b) OK
crosswordsFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordsFindRec ch pos tam [] = []
crosswordsFindRec ch pos tam (x:xs) | ((length x) == tam) && (x!!pos == ch) = [x] ++ crosswordsFindRec ch pos tam xs
								| otherwise = crosswordsFindRec ch pos tam xs								

-- d) OK
crosswordsFindN :: [Char] -> [Int] -> Int -> [String] -> [String]
crosswordsFindN [ch] [pos] tam xs = crosswordsFindLC ch pos tam xs
crosswordsFindN (ch:chs) (pos:poss) tam xs = crosswordsFindN chs poss tam (crosswordsFindLC ch pos tam xs)

-- 3
-- a) OK
countInstances :: Char -> [[Char]] -> [Int]
countInstances ch xs = [length [a | a <- x, a == ch] | x <- xs]

-- 4
-- b) OK
reverse_ :: [Int] -> [Int]
reverse_ [] = []
reverse_ (x:xs) = reverse_ xs ++ [x]

-- 5
-- a) OK
polyAdd_ :: [Int] -> [Int] -> [Int]
polyAdd_ xs [] = xs
polyAdd_ [] ys = ys
polyAdd_ (x:xs) (y:ys) = [(x+y)] ++ polyAdd_ xs ys

polyAdd xs ys = reverse (dropWhile (== 0) (reverse_ (polyAdd_ xs ys)))