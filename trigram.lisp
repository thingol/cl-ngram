;;; -*- Lisp -*-
;;;
;;; This code is based on the python-gram module
;;;

(in-package :org.kjerkreit.utils.ngram)

(declaim (ftype (function (string) list) tri-string-to-grams)
	 (ftype (function (string string) float) tri-compare-strings)
	 (ftype (function (list list) float) tri-compare-grams)
	 (inline tri-compare-grams))

(defun tri-string-to-grams (string)
  "Generates a list containing n-grams (as lists) based on a list of strings.
N defaults to 2. Strings are padded with (n-1) dollar signs ($)."

  (let ((s (concatenate 'string #\$ #\$ string #\$ #\$))
	(grams nil))

    (dotimes (i (- (length s) 2))
      (setf grams (append grams (list (subseq s i (+ i 2))))))

    grams))

(defun tri-compare-strings (string1 string2)
  "Compare two strings and return a score between 0 and 1."

  (tri-compare-grams (tri-string-to-grams string1)
		     (tri-string-to-grams string2)))

(defun tri-compare-grams (g1 g2)
  "Compare two sets of n-grams and return a score between 0 and 1."
  (declare (dynamic-extent g1 g2))

  (let ((shared 0)
	(tg2 g2)
	(len-g2 (list-length g2)))


    (map 'nil
	 #'(lambda (x)
	     (setf tg2 (remove x tg2 :test #'string= :count 1)))
	 g1)

    (float (/ (- len-g2
		 (list-length tg2))
	      (+ (list-length g1)
		 len-g2)))))
  