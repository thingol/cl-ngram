(unless (find-package 'org.kjerkreit.utils.ngram)
  (asdf:load-system 'org.kjerkreit.utils.ngram))

(use-package :org.kjerkreit.utils.ngram)

(defparameter *string0* "blablabla")
(defparameter *string1* "lablabla")
(defparameter *ngrams0* '("$b" "bl" "la" "ab" "bl" "la" "ab" "bl" "la" "a$"))
(defparameter *ngrams1* '("$l" "la" "ab" "bl" "la" "ab" "bl" "la" "a$"))

(defparameter *str-cmp0* (compare-strings *string0* *string1*))
(defparameter *str-cmp1* (compare-strings *string1* *string0*))
(defparameter *ng-cmp0* (compare-n-grams *ngrams0* *ngrams1*))
(defparameter *ng-cmp1* (compare-n-grams *ngrams1* *ngrams0*))



(format t "Assert: (compare-strings *string0* *string1*) => 0.72727275...~a~%"
	(= 0.72727275 *str-cmp0*))
(format t "Assert: (compare-strings *string1* *string0*) => 0.72727275...~a~%"
	(= 0.72727275 *str-cmp1*))
(format t "Assert: (compare-n-grams *ngrams0* *ngrams1*) => 0.72727275...~a~%"
	(= 0.72727275 *ng-cmp0*))
(format t "Assert: (compare-n-grams *ngrams1* *ngrams0*) => 0.72727275...~a~%"
	(= 0.72727275 *ng-cmp1*))

(quit)