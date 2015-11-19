(cl:in-package #:common-lisp-user)

(defpackage :sudoku-system
  (:use :asdf :common-lisp))

(in-package :sudoku-system)

(defparameter *sudoku-directory* (directory-namestring *load-truename*))
(format t "sudoku-directory is ~A ~%" *sudoku-directory*)

(asdf:defsystem :sudoku
  :serial t
  :components
  (
   (:file "classe")
   (:file "interactif")
   (:file "strategies")
   (:file "non-interactif")
   ))

(pushnew :sudoku *features*)

