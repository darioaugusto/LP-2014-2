;;;; Problema: dada uma lista de alunos e notas, retorne a lista de 
;;; alunos contendo o status de aprovado e reprovado considerando-se
;;; a mŽdia 6

(defparameter *banco-de-dados*
  '((A 7 8 9)
    (B 7 4 7)
    (C 8 4 8)
    (D 9 2 9)
    (E 4 5 6)))

(defvar *notas* *banco-de-dados*)

(defun media (nome)
  (/ (eval (append '(+) (rest (assoc nome *notas*)))) 3))

(defun status (nome)
  (if (>= (media nome) 6) (list nome "aprovado") (list nome "reprovado")))
  
(defun listoflists (func list)
  "aplica a funcao a cada elemento da lista de entrada e guarda os resultados em uma lista de listas"
  (apply #'list (mapcar func list)))
  
(listoflists #'status '(A B C D E F))