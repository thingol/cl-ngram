;;; -*- Lisp -*-

(in-package #:asdf)

(defsystem org.kjerkreit.utils.ngram
  :version "0.0.5"
  :author "Marius Hårstad Kjerkreit"
  :license "BSD-style"
  :components ((:static-file "LICENSE")
	       (:file "defpackage")
	       (:file "ngram" :depends-on ("defpackage"))))
