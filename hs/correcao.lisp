(defun eduardo (e l)
  (cond ((null l) nil)
        ((= e (first l)) t)
        (t (eduardo e (rest l)))))

(defun leonardo (x a)
  (if (length a > 0)
      (if (x == first a)
          (true)
          (leonardo ((rest x) a)))
     (false)))

(defun leonardo_r (x a)
  (if (> (length a) 0)
      (if (= x (first a))
          (t)
          (leonardo_r x (rest a)))
     nil))

(defun fernanda (x ys)
  (if (find x ys)
      t
      nil))

(defun otavio (x l)
  (cond (= l '()) nil
        (> (member x l) 0 ) (t)
        (= (member x l) 0 ) nil))

(defun vinicius (e l)
  (cond ((null l) nil)
        ((= e (first l)) t)
        (t (vinicius e (rest l)))))

(defun paulo (x a)
  (if (> (length a) 0)
      (if (= x (first a)) t (paulo x (rest a)))
      nil ))

(defun danielO (p l)
  (if (null l) '()
    (if (= (first l) p) t (danielO p (rest l)))))

(defun or2 (a b)
  (if a (if b t nil) nil))

(defun bernardo (x l)
  (cond ((null l) nil)
        ((= x (first l)) t)
        (t (or2 nil (bernardo x (rest l))))))

(bernardo 3 '(1 2 3))
(bernardo 4 '(1 2 3))

