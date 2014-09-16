
(defun make-maze-ops (pair)
  (list (make-maze-op (car pair) (cadr pair))
	(make-maze-op (cadr pair) (car pair))))

(defun make-maze-op (here there)
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
	   '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
	     (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
	     (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(use *maze-ops*)

(gps '((at 1)) '((at 25)))

