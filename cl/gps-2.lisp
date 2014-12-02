
(in-package :gps-2)

(defvar *ops* nil "a list of available operations.")

(defstruct op "an operation"
	   (action nil)
	   (preconds nil)
	   (add-list nil)
	   (del-list nil))

(defun executing-p (x)
  (starts-with x 'executing))

(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op))
	  (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  (convert-op (make-op :action action
		       :preconds preconds
		       :add-list add-list
		       :del-list del-list)))

(defun action-p (x)
  (or (equal x '(start)) (executing-p x)))


(defun gps (state goals &optional (*ops* *ops*))
  (find-all-if #'action-p 
	       (achieve-all (cons '(start) state) goals nil)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
   or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                  (find-all goal *ops* :test #'appropriate-p)))))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op) 
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x) 
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun use (oplist)
  (length (setf *ops* oplist)))


(defun find-path (start end)
  (let ((results (gps `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination 
			  (remove '(start) results :test #'equal))))))

(defun destination (action)
  (fifth (second action)))

