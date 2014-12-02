{-
Problema: dada uma lista de alunos e notas, retorne a lista de 
alunos contendo o status de aprovado e reprovado considerando-se
a média 6
-}

type TuplaAluno = (String,[Double])

aluno :: Int->TuplaAluno
aluno x
	|x==1 = ("Joao da Silva", [9.5,8.0,9.7])
	|x==2 = ("Maria de Souza",[7.5,6.0,7.7])
	|x==3 = ("Jose da Cunha", [6.0,5.7,5.5])
	|otherwise = ("Aluno Inexistente", [])

aprovacao :: TuplaAluno -> (String, String)
aprovacao tupla =
	if (media (snd tupla)) > 6
		then (fst tupla,"aprovado")
	else (fst tupla,"reprovado")

media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)
	
turma :: [Int] -> [(String,String)]
turma (x:xs) = [aprovacao( aluno x )] ++ turma xs
turma []=[]