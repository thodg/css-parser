
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
  ((:file "package")
   (:file "parser" :depends-on ("package"))
   (:file "input" :depends-on ("parser"))
   (:file "matcher" :depends-on ("parser"))
   (:file "tokenizer" :depends-on ("parser"))
   (:file "css-lexer" :depends-on ("input" "matcher" "tokenizer"))
   (:file "css-parser" :depends-on ("css-lexer"))))
