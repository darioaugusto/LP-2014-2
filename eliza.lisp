
(in-package :eliza)

(defun simple-equal (x y)
  "Don't check inside strings."
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))


(defun pat-match (pattern input)
  "Doest pattern match the input? Any variable can match anything."
  (if (variable-p pattern)
      t
      (if (or (atom pattern) (atom input))
	  (eql pattern input)
	  (and (pat-match (first pattern) (first input))
	       (pat-match (rest pattern) (rest input))))))

;; Variaveis sao simbolos iniciados com ?. Isto evita criação de
;; estruturas mais complexas. Simbolos são mais do que apenas
;; identificadores.

(defun variable-p (x)
  (and (symbolp x)
       (equal (char (symbol-name x) 0) #\?)))


;; (sublis '((?x . vacation)) '(what would it mean to you if you got a ?x ?))  
;;
;; logo precisamos que pat-match retorne uma tabela de variaveis e
;; valores, uma a-list não apenas t ou nil.

(defun pat-match (pattern input)
  "Version with bug."
  (if (variable-p pattern)
      (list (cons pattern input))
      (if (or (atom pattern) (atom input))
	  (eql pattern input)
	  (and (pat-match (first pattern) (first input))
	       (pat-match (rest pattern) (rest input))))))


;; Varios bugs na versao acima:
;;
;; 1. (eql pattern input) retorna lista
;;
;; 2. mesmo teste pode retornar nil para indicar falha mas será tratado
;;    como lista apenas.
;;
;; 3. Não conseguimos differenciar entre falha no teste versus
;;    inexistência de variável.
;;
;; 4. Duas occorrências da mesma variável precisam receber o mesmo valor.
;;
;; 5. se first falhar, para que continuar testando? Ineficiente.


(defconstant fail nil "pat-match failure.")

;; http://www.sbcl.org/1.0/manual/Defining-Constants.html#Defining-Constants
(defparameter no-bindings '((t . t)) 
  "pat-match success, with no variables.")


;; abstraindo da implementação usada para que a estrutura seja
;; facilmente trocada no futuro.

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val) bindings))


(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
	((variable-p pattern) 
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input)
			       bindings)))
	(t fail)))


(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))


(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings no-bindings)
	    nil
	    bindings)))


;; section 5.3 -- segment pattern matching with
;; http://en.wikipedia.org/wiki/Kleene_star variables (segment variables)

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
	((variable-p pattern) 
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-match pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input)
			       bindings)))
	(t fail)))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

;; the hard part, the segment-match

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	;; pat start with a constant, can't have 2 consecutive vars
	(let ((pos (position (first pat) input
			     :start start :test #'equal)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match pat (subseq input pos) bindings)))
		;; if fail, try another longer one. 
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    (match-variable var (subseq input 0 pos) b2))))))))


(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	;; pat start with a constant, can't have 2 consecutive vars
	(let ((pos (position (first pat) input
			     :start start :test #'equal)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match pat (subseq input pos) 
				   (match-variable var (subseq input 0 pos) bindings))))
		;; if fail, try another longer one. 
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

;; section 5.4

(defun rule-pattern (rule) (first rule))

(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))      
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))     
     (What would it mean if you got ?y)
     (Why do you want ?y ?) 
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y)) 
     (Do you really think its likely that ?y) 
     (Do you wish that ?y)
     (What do you think about ?y) 
     (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) 
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))       
     (Were you really?) 
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))     
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))     
     (What other feelings do you have?))))

(defun eliza ()
  (loop 
   (print 'eliza-cli>)
   (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  (some #'(lambda (rule)
	    (let ((result (pat-match (rule-pattern rule) input)))
	      (if (not (eq result fail))
		  (sublis (switch-viewpoint result)
			  (random-elt (rule-responses rule))))))
	*eliza-rules*))

(defun switch-viewpoint (words)
  (sublis '((I . you) (you . I) (me . you) (am . are) (are . am)) words))

