;;; -*- Lisp -*-
;;;
;;; This code is based on the python-gram module
;;;

(in-package :org.kjerkreit.utils.ngram)

(declaim (ftype (function (string) list) bi-string-to-grams)
	 (ftype (function (string string) float) bi-compare-strings)
	 (ftype (function (list list) float) bi-compare-grams)
	 (inline bi-compare-grams))

(defun bi-string-to-grams (string)
  "Generates a list containing n-grams (as lists) based on a list of strings.
N defaults to 2. Strings are padded with (n-1) dollar signs ($)."

  (let ((s (concatenate 'string #\$ string #\$))
	(grams nil))

    (dotimes (i (1- (length s)))
      (setf grams (append grams (list (subseq s i (1+ i))))))

    grams))

(defun bi-compare-strings (string1 string2)
  "Compare two strings and return a score between 0 and 1."

  (bi-compare-grams (bi-string-to-grams string1)
		    (bi-string-to-grams string2)))

(defun bi-compare-grams (g1 g2)
  "Compare two sets of n-grams and return a score between 0 and 1."
  (declare (dynamic-extent g1 g2))

  (let ((shared 0))
    (dolist (gram list1)
      (when (member gram list2 :test #'string=)
	(setf list2 (remove gram list2 :test #'string= :count 1))
	(incf shared)))

    (float (/ shared (+ (list-length list1)
			(list-length list2))))))
  