
(in-package :tableaux)


(defclass formula ()
  ((sign    :initform nil :initarg :sign :accessor formula-sign)
   (formula :initform nil :initarg :frm  :accessor formula-frm)))


(defun atomic? (formula)
  (not (listp (formula-frm formula))))

(defun is? (op formula)
  (equal op (car (formula-frm formula))))

(defun sign? (sign formula)
  (equal sign (formula-sign formula)))


(defun crash? (branch)
  (let ((atoms (remove-if-not #'atomic? branch)))
    ...))

(defun prove (formula)
  (let ((goal (make-instance 'formula :sign 'f :frm formula))) 
    (solve (list (list goal)))))

(defun solve (goals)
  (let ((current (remove-if #'atomic? (car goals)))) 
    (if (expand current))))


(defun expand (wfw)
  (cond 
    ((and (is? 'and wfw) (sign? 'f wfw))
     (let ((l (make-formula 'formula :sign 'f :frm (cadr  (formula-frm wfw))))
	   (r (make-formula 'formula :sign 'f :frm (caddr (formula-frm wfw)))))
       (list (list l) (list r))))
    ((and (is? 'and wfw) (sign? 't wfw))
     (let ((l (make-formula 'formula :sign 't :frm (cadr  (formula-frm wfw))))
	   (r (make-formula 'formula :sign 't :frm (caddr (formula-frm wfw)))))
       (list l r)))))

