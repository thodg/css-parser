
(in-package :common-lisp-user)

(defpackage :parse-css.system
  (:use :common-lisp :asdf))

(in-package :parse-css.system)

(defsystem :parse-css
  :name "parse-css"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "CSS level 3 parser"
  :depends-on ()
  :components
  ((:file "defpackage")
   (:file "lexer" :depends-on ("defpackage"))
   (:file "parser" :depends-on ("defpackage" "lexer"))))
