;;; -*- Lisp -*-
;;;
;;; This code is based on the python-gram module
;;;

(in-package :org.kjerkreit.ngram)

(declaim (optimize (speed 3) (safety 0)))

(defun gen-n-grams (strings &optional (n 2))
  "Generates a list containing n-grams (as lists) based on a list of strings.
N defaults to 2."
  (declare (ftype (function ((or string list) fixnum) list) gen-n-grams))
  
  (labels ((pad (string n)
	     (concatenate 'string
			  (make-string (1- n) :initial-element #\$)
			  string
			  (make-string (1- n) :initial-element #\$)))
	   (chop-string (string)
	     "Divides a string into grams."

	     (let ((s (pad string n))
		   (grams nil))
	       (declare (string s)
			(list grams))

	       (dotimes (i (- (length s) (1- n)))
		 (setf grams (append grams (list (subseq s i (+ i n))))))

	       grams)))

    (declare (ftype (function (string fixnum) string) pad)
	     (ftype (function (string) list) chop-string))

    (if (listp strings)
	(progn
	  (let ((n-grams))
	    (dolist (string strings)
	      (setf n-grams (append n-grams (list (chop-string string)))))))
	(chop-string strings))))

(defun compare-strings (string1 string2 &optional (warp 1d0) (n 2))
  "Compare two strings and return a score between 0 and 1."
  (declare (ftype (function (string string float fixnum) float) compare-strings))

  (compare-n-grams (gen-n-grams string1 n) (gen-n-grams string2 n) warp))

(defun compare-n-grams (g1 g2 &optional (warp 1d0))
  "Compare two sets of n-grams and return a score between 0 and 1."
  (declare (ftype (function (list list double-float) double-float) compare-n-grams))
  (declare (double-float warp))

  (flet ((compare-members (list1 list2)
	     "How many grams are shared, and how many are there in total?"
	     (declare (list list1)
		      (list list2))

	     (let ((shared 0))
	       (declare (fixnum shared))
	       (dolist (gram list1)
		 (declare (string gram))
		 (when (member (the string gram) (the list list2) :test #'string=)
		   (setf list2 (the list (remove (the string gram) (the list list2) :test #'string= :count 1)))
		   (incf shared)))

	       (cons shared (the fixnum (+ (list-length list1) (list-length list2)))))))

    (declare (ftype (function (list list) cons) compare-members))
    
    (let* ((gram-stats (compare-members g1 g2))
	   (shared (car gram-stats))
	   (total (cdr gram-stats)))
      (declare (cons gram-stats)
	       (fixnum shared)
	       (fixnum total))

      (if (< (abs (- warp 1.0)) 1e-9)
	  (/ (float shared) total)
	  (the double-float (/ (- (the double-float (* total warp))
		(the double-float (* (- total shared) warp)))
	     (the double-float (* total warp))))))))
