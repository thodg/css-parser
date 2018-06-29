
(in-package :common-lisp-user)

(defpackage :css-parser.system
  (:use :common-lisp :asdf))

(in-package :css-parser.system)

(defsystem :css-parser
  :name "css-parser"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.2"
  :description "CSS level 3 parser"
  :depends-on ("css-lexer" "parser-stream" "str")
  :components
  ((:file "package")
   (:file "css-parser" :depends-on ("package"))))

(defsystem :css-parser/test
  :depends-on ("babel-stream"
               "css-parser"
               "unistd-stream")
  :components
  ((:file "test")))
