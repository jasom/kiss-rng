;;;; package.lisp

(defpackage #:kiss-rng
  (:use #:cl)
  (:export :make-superkiss-state
           :sks-init state
           :kiss
           :make-superkiss64-state
           :sks64-init
           :kiss64))

