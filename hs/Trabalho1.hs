{-
Dada uma lista de tuplas de filmes contendo:
[(codigo, titulo, diretor, ano, [atores], genero, [notas])]
implemente funções que:
1) encontre e retorne a tupla de título T
2) encontre e retorne a lista de tuplas do genero G
3) ordene e retorne a lista de tuplas por titulo, diretor, ano
(o campo de ordenação é um parametro da funcao)
4) busque e retorne uma lista de filmes que contem determinado 
ator na lista de atores
5) selecione filmes com média de notas superior a N ordenados pela média.
-}

type Filme = (Int, String, String,Int,[String],String,[Double])

film :: Int->Filme
film x
	|x==1 = (1,"O Poderoso Chefao", "Coppola", 1972,["Ator1", "Ator2"],"Drama",[9.5,8.0,9.7])
	|x==2 = (2,"Melancholia", "Von Trier", 2011,["Ator1", "Ator2", "Ator3"],"Suspense",[7.5,6.0,7.7])
	|x==3 = (3,"Se eu fosse voce", "Daniel Filho", 2006,["Ator1", "Ator2"],"Comedia",[6.0,5.7])
	|otherwise = (0,"Não existe filme com esse id","",0,[],"", [])
