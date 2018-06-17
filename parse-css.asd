
(in-package :common-lisp-user)

(defpackage :parse-css.system
  (:use :common-lisp :asdf))

(in-package :parse-css.system)

(defsystem :parse-css
  :name "parse-css"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.2"
  :description "CSS level 3 parser"
  :depends-on ("css-lexer" "str")
  :components
  ((:file "package")
   (:file "parser" :depends-on ("package"))
   (:file "css-parser" :depends-on ("parser"))))

(defsystem :parse-css/test
  :depends-on ("babel-stream"
               "parse-css"
               "unistd-stream")
  :components
  ((:file "test")))
