-- converts from char to num
let2int :: Char -> Int
let2int c = fromEnum c - fromEnum 'a'

-- converts from num to char
int2let :: Int -> Char
int2let n = toEnum (fromEnum 'a' + n)

pertence  :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (y:ys) = x==y || pertence x ys

isLower :: Char -> Bool
isLower c = pertence c ['a'..'z']

-- implements the circular shift
--   ------------------>
-- ['a','b', .. , 'w','z']
--   ^------------------
shift :: Int -> Char -> Char
shift n c | isLower c = int2let(mod (let2int c + n) 26)
		  | otherwise = c
		  
encode :: Int -> String -> String
encode n xs = [shift n x | x <-xs] 
		
--table :: [Float]
--table = [8.2 1.5 ...... 8.7]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / (fromIntegral m))*100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) length xs | x <- ['a'..'z']]