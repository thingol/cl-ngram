;;; -*- Lisp -*-
;;;
;;; This code is based on the python-gram module
;;;

(in-package :org.kjerkreit.utils.ngram)

(declaim (ftype (function (list &optional fixnum) list) gen-n-grams)
	 (ftype (function (string string &optional float fixnum) float) compare-strings)
	 (ftype (function (list list &optional float) float) compare-n-grams))

(defun gen-n-grams (strings &optional (n 2))
  "Generates a list containing n-grams (as lists) based on a list of strings.
N defaults to 2. Strings are padded with (n-1) dollar signs ($)."

  (labels ((pad (string n)
	     (declare (ftype (function (string fixnum) string) pad))
	     
	     (concatenate 'string
			  (make-string (1- n) :initial-element #\$)
			  string
			  (make-string (1- n) :initial-element #\$)))
	   (chop-string (string)
	     (declare (ftype (function (string) list) chop-string))
	     "Divides a string into grams."

	     (let ((s (pad string n))
		   (grams nil))

	       (dotimes (i (- (length s) (1- n)))
		 (setf grams (append grams (list (subseq s i (+ i n))))))

	       grams)))

    (let ((n-grams))
      (dolist (s strings)
	(setf n-grams (append n-grams (list (chop-string s)))))
      n-grams)))

(defun compare-strings (string1 string2 &optional (warp 1.0) (n 2))
  "Compare two strings and return a score between 0 and 1."

  (compare-n-grams (car (gen-n-grams (list string1) n)) (car (gen-n-grams (list string2) n)) warp))

(defun compare-n-grams (g1 g2 &optional (warp 1.0))
  "Compare two sets of n-grams and return a score between 0 and 1."
  
  (labels ((compare-members (list1 list2)
	     (declare (ftype (function (list list) cons) compare-members))
	     "How many grams are shared, and how many are there in total?"

	     (let ((shared 0))
	       (dolist (gram list1)
		 (when (member gram list2 :test #'string=)
		   (setf list2 (remove gram list2 :test #'string= :count 1))
		   (incf shared)))

	       (cons shared (+ (list-length list1) (list-length list2))))))
    
    (let* ((gram-stats (compare-members g1 g2))
	   (shared (car gram-stats))
	   (total (cdr gram-stats)))

      (if (< (abs (- warp 1.0)) 1e-9)
	  (float (/ shared total))
	  (/ (- (* total warp)
		(* (- total shared) warp))
	     (* total warp))))))
