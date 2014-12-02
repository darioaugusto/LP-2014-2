; Gabarito Dario
; 1
; c)
(defun crosswords-find (ch pos tam list)
	(cond ((null list) '())
		  ((and (equal tam (length (car list))) (equal (nth pos (car list)) ch)) (append (list (car list)) (crosswords-find ch pos tam (cdr list))))
		  (t (crosswords-find ch pos tam (cdr list)))))
; d)
  
(defun crosswords-findN_ (chs poss tam list)
		 (LOOP FOR ch IN chs AND pos IN poss
			COLLECT (crosswords-find ch pos tam list)))


(defun full-intersection (list)
	(cond ((= (length list) 2) (intersection (first list) (second list)))
	(t (intersection (first list) (full-intersection (cdr list))))))
		 
(defun crosswords-findN (chs poss tam list)
		 (full-intersection (crosswords-findN_ chs poss tam list)))

; 2 - TODO

; 3
; b)
(defun count-instances (obj lsts)
	(labels ((instances-in (lst)
				(if (consp lst)
					(if (consp (car lst))
						(+ (instances-in (car lst)) (instances-in (cdr lst)))
						(+ (if (eq (car lst) obj) 1 0) (instances-in (cdr lst)))) 0)))
		(mapcar #'instances-in lsts)))
	
	
; 4
; a)
(defun reverse_ (lst)
	(labels ((rev (lst acc)
				(if (null lst) acc
					(rev (cdr lst) (append (list (car lst)) acc)))))
			(rev lst nil)))
			
; 5
; b)

(defun poly-add_ (eq1 eq2)
	(cond ((null eq1) eq2)
		  ((null eq2) eq1)
		  (t (append (list (+ (first eq1) (first eq2))) (poly-add_ (cdr eq1) (cdr eq2))))))

(defun filter-zeros (eq_)
	(if (equal (last eq_) (list 0)) (filter-zeros (butlast eq_)) eq_))
			  
(defun poly-add (eq1 eq2)
	(filter-zeros (poly-add_ eq1 eq2)))
	
	 (poly-add '(-6 4 9) '(3 7 -9))