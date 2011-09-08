;;; -*- Lisp -*-

(in-package :cl-user)

(defpackage #:org.kjerkreit.utils.ngram-system
  (:use #:asdf #:cl))

(in-package :org.kjerkreit.utils.ngram-system)

(defsystem org.kjerkreit.utils.ngram
  :version "0.0.4"
  :author "Marius Hårstad Kjerkreit"
  :license "BSD-style"
  :components ((:static-file "LICENSE")
	       (:file "defpackage")
	       (:file "ngram" :depends-on ("defpackage"))))
