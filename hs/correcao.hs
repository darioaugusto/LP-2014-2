eduardo e [] = False
eduardo e (x:xs) | e == x = True
				 | otherwise = eduardo e xs
				 
--fernanda x [x] = True
fernanda x [] = False
fernanda x (y:ys) = if x /= y then (fernanda x ys) else True

rafael :: Ord a => a -> [a] -> Bool
rafael a [] = False
--rafael a (x:xs) = if (a) = [x] True else rafael a (xs)
rafael a (x:xs) = if a == x then True else rafael a xs

--otavio (x:xs) n = | x = n == True
otavio [] n = False
otavio (x:xs) n | x == n = True
			    | otherwise = otavio (xs) n
			    
vinicius e [] = False
vinicius e lista = if (e == (head lista))
					then True
					else vinicius e (tail lista)
					
paulo:: Ord a => a -> [a] -> Bool
paulo b [] = False
paulo b (x:xs) =
	if (b==x) 
		then True
	else
		paulo b xs
		
daniel_oli p [] = False
daniel_oli p (x:xs) = if p == x then True else daniel_oli p xs

bernardo x [] = False
bernardo x (a:as) = (x==a || bernardo x as)