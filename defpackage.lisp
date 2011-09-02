(in-package :cl-user)

(defpackage :org.kjerkreit.utils.ngram
  (:nicknames "ngram")
  (:use :common-lisp)
  (:export gen-n-grams
	   compare-strings
	   compare-n-grams))

