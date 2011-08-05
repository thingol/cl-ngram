;;; -*- Lisp -*-
;;;
;;; This code is based on the python-gram module
;;;

(in-package :org.kjerkreit.ngram)

(defun gen-n-grams (strings &optional (n 2))
  "Generates a list containing n-grams (as lists) based on a list of strings.
N defaults to 2."

  (flet ((pad (string n)
	   "Pads the strings before processing."
	   	     
	   (concatenate 'string
			(make-string (1- n) :initial-element #\$)
			string
			(make-string (1- n) :initial-element #\$)))

	 (chop-string (string)
	   "Divides a string into grams."

	   (let ((s (pad string n))
		 (grams nil))
	     
	     (dotimes (i (- (length s) (1- n)))
	       (setf grams (append grams (list (subseq s i (+ i n))))))
	     
	     grams)))
    	 
    (if (listp strings)
	(do ((ngrams nil (append ngrams (list (chop-string (car s)))))
	     (s strings (cdr s)))
	    ((not strings) ngrams))
	(chop-string strings))))

(defun compare-strings (string1 string2 &optional (warp 1.0) (n 2))
  "Compare two strings and return a score between 0 and 1."

  (compare-n-grams (gen-n-grams string1 n) (gen-n-grams string2 n) warp))

(defun compare-n-grams (g1 g2 &optional (warp 1.0))
  "Compare two sets of n-grams and return a score between 0 and 1."

  (flet ((compare-members (list1 list2)
	   "How many grams are shared, and how many are there in total?"

	   (do ((shared 0)
		(l list1 (cdr l)))
	       
	       ((not l)
		(cons shared (+ (list-length list1)
				(list-length list2))))
	     
	     (when (member (car l) list2 :test #'equal)
	       (setf list2 (remove  (car l) list2 :test #'equal :count 1))
	       (setf shared (1+ shared))))))
    
    (let* ((gram-stats (compare-members g1 g2))
	   (shared (car gram-stats))
	   (total (cdr gram-stats)))

      (if (< (abs (- warp 1.0)) 1e-9)
	  (float (/ shared total))
	  (float (/ (- (* total warp)
		       (* (- total shared) warp))
		    (* total warp)))))))
