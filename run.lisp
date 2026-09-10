;;; run.lisp — Quick loader for cl-harness
;;; Usage: sbcl --load ~/quicklisp/setup.lisp --load run.lisp

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-harness)
(in-package :cl-harness)
(main)