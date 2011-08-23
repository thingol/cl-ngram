;;; -*- Lisp -*-

(in-package :cl-user)

(defpackage #:org.kjerkreit.ngram-system
  (:use #:asdf #:cl))

(in-package :org.kjerkreit.ngram-system)

(defsystem org.kjerkreit.ngram
  :version "0.0.2"
  :author "Marius Hårstad Kjerkreit"
  :license "BSD-style"
  :components ((:static-file "LICENSE")
	       (:file "defpackage")
	       (:file "ngram" :depends-on ("defpackage"))))
