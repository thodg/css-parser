
(in-package :common-lisp-user)

(defpackage :css-parser
  (:use :cl-stream
        :common-lisp
        :css-lexer
        :parser-stream
        :str)
  #.(cl-stream:shadowing-import-from)
  (:export
   #:css-item
   #:parse-stylesheet
   #:css-parser))
