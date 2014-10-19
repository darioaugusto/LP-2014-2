
;; section 5.2

;; Exercise 5.1?

(pat-match '(I need a ?x) '(I need a vacation))

(sublis (pat-match '(I need a ?x) '(I need a vacation))
	'(what would it mean to you if you got a ?x ?))

(pat-match '(I need a ?x) '(I really need a vacation))

(pat-match '(this is easy) '(this is easy))

(pat-match '(?x is ?x) '((2 + 2) is 4))

(pat-match '(?x is ?x) '((2 + 2) is (2 + 2)))

(pat-match '(?p need . ?x) '(I need a long vacation))

;; section 5.3

(pat-match '((?* ?p) need (?* ?x)) '(Mr Alex and I need a vacation))

(pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool))

;; try trace the pat-match and segment-match

;; the following fails!

(pat-match '((?* ?x) a b (?* ?x)) '(1 2 a b a b 1 2 a b))

;; how to debug in SBCL, trace no SBCL funciona
;; trace no mlisp?

http://www.sbcl.org/manual/#Debugger-Policy-Control
http://stackoverflow.com/questions/8617064/a-simple-example-of-using-the-stepper-in-sbcl


