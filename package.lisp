
(in-package :common-lisp-user)

(defpackage :parse-css
  (:use :cl-stream
        :common-lisp
        :css-lexer
        :str)
  #.(cl-stream:shadowing-import-from)
  (:export
   #:css-item
   #:parse-stylesheet
   #:css-parser))
