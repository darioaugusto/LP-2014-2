
(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
	(unless (equal a b)
	  (dolist (c blocks)
	    (unless (or (equal c a) (equal c b))
	      (push (move-op a b c) ops)))
	  (push (move-op a 'table b) ops)
	  (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))


(use (make-block-ops '(a b)))

(gps '((a on table) (b on table) (space on a) (space on b) (space on table))
     '((a on b) (b on table)))

(gps '((a on b) (b on table) (space on a) (space on table))
     '((b on a)))

(use (make-block-ops '(a b c)))

(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((b on a) (c on b)))

;; order matter
(gps '((a on b) (b on c) (c on table) (space on a) (space on table))
     '((c on b) (b on a)))

