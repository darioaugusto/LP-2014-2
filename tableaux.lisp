
(in-package :tableaux)


(defclass formula ()
  ((sign    :initform nil :initarg :sign :accessor formula-sign)
   (frm     :initform nil :initarg :frm  :accessor formula-frm)))


(defun atomic? (formula)
  (not (listp (formula-frm formula))))

(defun make-formula (sign wfw)
  (make-instance 'formula :sign sign :frm wfw))

(defun is? (op formula)
  (equal op (car (formula-frm formula))))

(defun sign? (sign formula)
  (equal sign (formula-sign formula)))


(defun crash? (branch)
  (let ((atoms (remove-if-not #'atomic? branch)))
    ...))


(defun expand (formula)
  (cond 
    ((and (is? 'and formula) (sign? 'f formula))
     (let ((lhs (make-formula 'f (cadr  (formula-frm formula))))
	   (rhs (make-formula 'f (caddr (formula-frm formula)))))
       (list (list lhs) (list rhs))))
    ((and (is? 'and formula) (sign? 't formula))
     (let ((lhs (make-formula 't (cadr  (formula-frm formula))))
	   (rhs (make-formula 't (caddr (formula-frm formula)))))
       (list lhs rhs)))
    ...))


(defun solve (goals)
  (let ((current (remove-if #'atomic? (car goals)))) 
    ...))


(defun prove (formula)
  (let ((goal (make-instance 'formula :sign 'f :frm formula))) 
    (solve (list (list goal)))))
