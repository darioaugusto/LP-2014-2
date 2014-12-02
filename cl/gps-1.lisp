
(in-package :gps-1)

(defvar *state* nil "the current state: a list of conditions.")

(defvar *ops* nil "a list of available operations.")

(defstruct op "an operation"
	   (action nil)
	   (preconds nil)
	   (add-list nil)
	   (del-list nil))

(defun gps (*state* goals *ops*)
  "General Problem Solver"
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  (or (member goal *state*)
      (some #'apply-op
	    (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  (member goal (op-add-list op)))

(defun apply-op (op)
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))
