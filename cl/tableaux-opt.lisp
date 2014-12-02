
(in-package :tableaux)

(defun split (branches)
  (labels ((split-aux (branches derivable non-derivable)
	     (if (null branches)
		 (values derivable non-derivable)
		 (let ((b (car branches)))
		   (if (full-expanded? b)
		       (split-aux (cdr branches) derivable (cons b non-derivable))
		       (values (cons b derivable) 
			       (append (cdr branches) non-derivable)))))))
    (split-aux branches nil nil)))



