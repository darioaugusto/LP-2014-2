-- Aula 4

--funcao saque
saque :: Int -> [(Int,Int)]
saque x  
	| (mod x 2 == 1) =[(1,5)] ++ saque (x - 5)
	| x >= 100 = [(div x 100,100)] ++ saque (mod x ((div x 100)*100))
	| (x < 100) && (x>=50) = [(div x 50,50)] ++ saque (mod x ((div x 50)*50))
	| (x < 50) && (x>=20) = [(div x 20,20)] ++ saque (mod x ((div x 20)*20))
	| (x < 20) && (x>=10) = [(div x 10,10)] ++ saque (mod x ((div x 10)*10))
	| (x < 10) && (x>=2) = [(div x 2,2)] ++ saque (mod x ((div x 2)*2))
	| otherwise = []

-- funcao soma impares de 1 a N	
somaI::Int->Int
somaI x = somaAux1 x 0

somaAux1::Int->Int->Int
somaAux1 x y
	|x==1 = y+x
	|odd x = somaAux1 (x-2) (y+x)
	|otherwise = somaAux1 (x-1) y

-- funcao inverte
inverte [] = []
inverte (a:b) = (inverte b) ++ [a]

-- funcao length	
conta :: [Int] -> Int
conta [] = 0
conta (c : r) = 1 + conta r

-- funcao maior elemento
maior [a] = a
maior (a : t) = 
	if (a > (maior t))
	 then a 
	else (maior t)

-- funcoes de primeiro e segundo graus
primeiro_grau::Float->Float->Float
primeiro_grau a b = - b / a

delta::Float->Float->Float->Float
delta a b c = -b^2 * 4 * a * c

segundo_grau::Float->Float->Float->(Float, Float)
segundo_grau a b c = 
	if (delta a b c) < 0.0 
		then undefined
 	else ( (- b - sqrt(delta a b c)) / 2.0 * a,
 (- b + sqrt(delta a b c)) / 2.0 * a)

-- retorna elementos maiores que X
maiores :: Ord a => [a] -> a -> [a]
maiores (y:ys) x =
	if (ys == [])
		then []
	else
		if (y > x) 
			then [y] ++ maiores ys x
		else [] ++ maiores ys x
		
-- Desafio
{- 
Criar uma lista de tuplas de alunos (Nome, Nota1, Nota2, Nota3) e devolver
uma outra tupla (Nome,status) onde status contem Aprovado/Reprovado considerando-se
a média 6.0.
-}