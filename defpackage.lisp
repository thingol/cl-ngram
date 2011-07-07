(in-package :cl-user)

(defpackage :org.kjerkreit.ngram
  (:nicknames "ngram")
  (:use :common-lisp)
  (:export gen-n-grams
	   compare-strings
	   compare-n-grams))

