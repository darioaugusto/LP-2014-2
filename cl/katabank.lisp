;;; User Story 1
; The match-numbers function is called '(match-numbers *file*)' and
; returns the numbers written in the file.

;;; User Story 2
; The check-numbers function gets the list of numbers as an argument
; and returns 't' for a valid number and 'nil' otherwise.

;;; User Story 3

(in-package :katabank)

(defparameter *bin-nums*
  '((("000" "001" "001") . 1)
    (("020" "021" "120") . 2)
    (("020" "021" "021") . 3)
    (("000" "121" "001") . 4)
    (("020" "120" "021") . 5)
    (("020" "120" "121") . 6)
    (("020" "001" "001") . 7)
    (("020" "121" "121") . 8)
    (("020" "121" "021") . 9)
    (("020" "101" "121") . 0)))


(defun match-numbers (blk &key (convert nil))
  (let ((numbers (mapcar #'(lambda (x) (cdr (assoc x *bin-nums* :test #'equal))) 
			 blk)))
    (if convert
	(read-from-string (format nil "~{~a~}" numbers))
	numbers)))


(defun adjust-lines (lines)
  (apply #'mapcar #'list lines))


(defun make-it-binary (line)
  (coerce (sublis '((#\_ . #\2) (#\| . #\1) (#\Space . #\0)) 
		  (loop for x across line collect x)
		  :test #'equal) 
	  'string))


(defun split-line (line)
  (mapcar #'(lambda (i) (subseq line i (+ 3 i))) '(0 3 6 9 12 15 18 21 24)))

;; (defun read-numbers (lines)
;;   (match-numbers (adjust-lines (mapcar (compose #'split-line #'make-it-binary) lines))))

(defun read-numbers (lines)
  (funcall (compose #'match-numbers #'adjust-lines) 
	   (mapcar (compose #'split-line #'make-it-binary) lines)))

(defun read-file (filename)
  (with-open-file (in filename) 
    (do ((state 0 (mod (1+ state) 4))
	 (blk nil)
	 (blocks nil)
	 (line (read-line in nil nil) 
	       (read-line in nil nil)))
	((null line)
	 (progn 
	   (push (read-numbers (reverse blk)) blocks) 
	   (reverse blocks)))
      (dbg :katabank "line: ~s state: ~s~%" line state)
      (cond 
	((< state 3)
	 (push line blk))
	(t
	 (progn (push (read-numbers (reverse blk)) blocks)
		(setf blk nil)))))))


(defun check-numbers (numbers)
  (= (mod (apply #'+
		 (mapcar #'* '(9 8 7 6 5 4 3 2 1) numbers)) 11) 0))


(defun save-log (input-file log-file)
  (with-open-file (log log-file :direction :output :if-exists :supersede)
    (let ((numbers (read-file input-file)))
      ; TODO 
      )))

