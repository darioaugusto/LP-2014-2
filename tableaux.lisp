
(in-package :tableaux)


(defclass formula ()
  ((sign    :initform nil :initarg :sign :accessor formula-sign)
   (frm     :initform nil :initarg :frm  :accessor formula-frm)))


(defun atomic? (formula)
  (not (listp (formula-frm formula))))

(defun make-formula (sign wff)
  (make-instance 'formula :sign sign :frm wff))

(defun is? (op formula)
  (equal op (car (formula-frm formula))))

(defun sign? (sign formula)
  (equal sign (formula-sign formula)))


(defun crash? (branch)
  (let ((atoms (remove-if-not #'atomic? branch)))
    (loop for atom = (car atoms) then (car others)
	  for others = (cdr atoms) then (cdr others)
	  (if (remove-if-not (lambda (o-frm)
			       (and (not (sign? (formula-sign atom) o-frm))
				    (equal (formula-frm atom) 
					   (formula-frm o-frm))))
			     others)))))


(defun expand (formula branch)
  (cond
    ((and (is? 'and formula) (sign? 'f formula)) 
     (let ((lhs (make-formula 'f (cadr  wff)))
	   (rhs (make-formula 'f (caddr wff))))
       (list (cons lhs branch) (cons rhs branch))))
    ((and (is? 'and formula) (sign? 't formula)) 
     (let ((lhs (make-formula 't (cadr  wff)))
	   (rhs (make-formula 't (caddr wff))))
       (list (append (list lhs rhs) branch))))
    ((and (is? 'or formula) (sign? 't formula))
     (let ((lhs (make-formula 't (cadr  wff)))
	   (rhs (make-formula 't (caddr wff))))
       (list (cons lhs branch) (cons rhs branch))))
    ((and (is? 'or formula) (sign? 'f formula))
     (let ((lhs (make-formula 'f (cadr  wff)))
	   (rhs (make-formula 'f (caddr wff))))
       (list (cons lhs branch) (cons rhs branch))))
    ((and (is? 'not formula) (sign? 'f formula)) 
     (list (cons (make-formula 'f (cadr wff)) branch)))
    ((and (is? 'not formula) (sign? 't formula)) 
     (list (cons (make-formula 't (cadr wff)) branch)))
    (t nil)))


(defun solve (branches)
  (let ((non-atomics (remove-if #'atomic? (car branches)))) 
    (if (null non-atomics)
	non-atomics
	(let ((a-form (car current)))
	  (solve (append (expand a-form) (cdr branches)))))))


(defun prove (wff)
  (solve (list (list (make-formula 'f wff)))))
