(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article () (one-of '(the a)))
(defun Noun () (one-of '(man ball woman table)))
(defun Verb () (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a list of it"
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random"
  (elt choices (random (length choices))))

(defun Adj* ()
  (if (= (random 2) 0 )
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))

(defun PP () (append (Prep) (noun-phrase)))

(defun Adj () (one-of '(big little blue green)))
(defun Prep () (one-of '(to in by with)))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hot took saw liked))
  "A grammar for a trivial subset in English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate")

(assoc 'noun *grammar*)

(defun rule-lhs (rule)
  "The left-hand side of a rule"
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule"
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun mappend (func list)
  "aplica a funcao a cada elemento da lista e concatena com o resultado"
  (apply #'append (mapcar func list)))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Adj* -> () (Adj Adj*))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hot took saw liked))
  "A grammar for a trivial subset in English.")

(setf *grammar* *bigger-grammar*)

(generate 'sentence)

