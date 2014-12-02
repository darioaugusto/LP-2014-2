--Aula 3

--funcao par: diferentes implementacoes
par :: Int -> Bool
par b = ((b `mod` 2) == 0)

par2 :: Int -> Bool
par2 x
	| mod x 2 == 0 = True
	| otherwise = False

par3 :: Int -> Bool	
par3 n =
  if mod n 2 == 0
    then True
  else
    False
    
--funcao pertence: diferentes implementacoes
pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x ys = any (==x) ys

pertence2 :: Eq a => a -> [a] -> Bool
pertence2 y [x] = x==y
pertence2 x ys = x==head(ys) || pertence2 x (tail(ys))

pertence3  :: Eq a => a -> [a] -> Bool
pertence3 x [] = False
pertence3 x (y:ys) = x==y || pertence3 x ys

{-
Notas da aula:

1) Ambiente de desenvolvimento

máquina virtual com haskell: https://www.fpcomplete.com/page/haskell-eval-vm

2) Retomada dos capitulos 1, 2 e 3

Linguagem funcional, OO, imperativa, declarativa
Revisão dos tipos; funções disponíveis no Prelude.
Recursão, pilha de execução.

3) Execução de exemplos e discussão de diferenças

4) Problemas para os alunos fazerem no quadro em conjunto e discussão sobre 
as diferentes implementações
a) funcao par
b) funcao pertence

5) Trabalho para trazer pra próxima aula

Simular um caixa eletronico que recebe um valor como entrada e retorna um vetor com 
a contagem das notas de 2, 5, 10, 20, 50 e 100 reais. O critério é retornar a menor
contagem total de notas que perfaz o valor. 

-}