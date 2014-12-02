-- Aula 2: Exercícios

-- # Capitulo 1
-- ex 1

-- # Capitulo 2
-- ex 3

n = a `div` length xs
	where
		a=10
		xs=[1,2,3,4,5]

last_ xs = take 1 (reverse(xs))

second xs = head(tail(xs))

swap(x,y) = (y,x)

pair x y = (x,y)

double x = x*2

palindrome xs = reverse xs == xs

twice f x = f(f(x))


-- verifica se eh par
par :: Int -> Bool
par x
	| mod x 2 == 0 = True
	| otherwise = False
	
	
zip2 (x:xs) (y:ys) = (x,y) : zip xs ys
zip2 _ [] = []
zip2 [] _ = []

pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x ys = any (==x) ys

potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x y = x * potencia x (y-1)
	 