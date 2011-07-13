;;; -*- Lisp -*-
;;;
;;; This code is based on the python-gram module
;;;

(in-package :org.kjerkreit.ngram)

(defun gen-n-grams (strings &optional (n 2))
  "Generates a list containing n-grams (as lists) based on a list of strings.
N defaults to 2."

  (defun pad (string n)
    "Pads the strings before processing."

    (concatenate 'string
		 (make-string (1- n) :initial-element #\$)
		 string
		 (make-string (1- n) :initial-element #\$)))

  (defun chop-string (string)
    (let ((s (pad string n))
	  (grams nil))
      (dotimes (i (- (length s) (1- n)))
	(setf grams (append grams (list (subseq s i (+ i n))))))
      grams))
	    
  
  (defun iter (strings n-grams)
    "Iterates over the list of strings to produce the grams."

    (if strings
	(iter (cdr strings) (append n-grams (list (chop-string (car strings)))))
	n-grams))

  (if (listp strings)
      (iter strings nil)
      (chop-string strings)))

(defun compare-strings (string1 string2 &optional (warp 1.0) (n 2))
  "Compare two strings and return a score between 0 and 1."

  (compare-n-grams (gen-n-grams string1 n) (gen-n-grams string2 n) warp))

(defun compare-n-grams (g1 g2 &optional (warp 1.0))
  "Compare two sets of n-grams and return a score between 0 and 1."

  (defun compare-members (list1 list2)
    "How many grams are shared, and how many are there in total?"

    (defun iter (list1 shared)
      (if list1
	  (progn
	    (when (member (car list1) list2 :test #'equal)
	      (setf list2 (remove  (car list1) list2 :test #'equal :count 1))
	      (setf shared (1+ shared)))
	    (iter (cdr list1) shared))
	  shared))

    (cons (iter list1 0) (+ (list-length list1) (list-length list2))))

  (let* ((gram-stats (compare-members g1 g2))
	 (shared (car gram-stats))
	 (total (cdr gram-stats)))

    (if (< (abs (- warp 1.0)) 1e-9)
	(float (/ shared total))
	(float (/ (- (* total warp)
		     (* (- total shared) warp))
		     (* total warp))))))
