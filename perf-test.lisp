(in-package :cl-user)

(asdf:load-system 'org.kjerkreit.ngram)
(asdf:load-system 'org.kjerkreit.lingalyzer)

(defpackage :org.kjerkreit.ngram-perftest
  (:nicknames "ngram-pt")
  (:use :org.kjerkreit.ngram
	:org.kjerkreit.lingalyzer.utils)
  (:import-from :org.kjerkreit.ngram
		:compare-n-grams
		:gen-n-grams)
  (:import-from :org.kjerkreit.lingalyzer
		:read-file))

(defparameter *input-data*  "lingalyzer/test-data/de-bello-gallico-01-08-latin-library.txt")
(defparameter *grams* (org.kjerkreit.ngram:gen-n-grams
		 (org.kjerkreit.lingalyzer.utils:read-file *input-data*)))
(defparameter *gl* (list-length *grams*))
(defparameter *matrix* (make-array `(,*gl* ,*gl*) :element-type 'float))

(defmacro labeled-time (form)
  `(progn
    (format *trace-output* "~2&~a" ',form)
    (time ,form)))

(defun run-test ()

    (do ((l1 (cdr *grams*) (cdr l1))
	 (g1 (car *grams*) (car l1))
	 (i 0 (1+ i)))
	((not l1))
      (do ((l2 (cdr *grams*) (cdr l2))
	   (g2 (car *grams*) (car l2))
	   (j 0 (1+ j)))
	  ((not l2))
	(setf (aref *matrix* i j) (org.kjerkreit.ngram:compare-n-grams g1 g2)))))

;;(labeled-time (run-test))
	 
	   




  
