
(in-package :common-lisp-user)

(defpackage :css-parser/test
  (:use :babel-stream
        :cl-stream
        :common-lisp
        :css-lexer
        :css-parser
        :unistd-stream)
  #.(cl-stream:shadowing-import-from)
  (:export
   #:run
   #:simple-test
   #:test-file))

(in-package :css-parser/test)

(defun simple-test ()
  (with-stream (css (css-parser
                     (css-lexer
                      (string-input-stream
                       "body { color: #f00; }"))))
    (stream-read css)))

(defun test-file (path)
  (with-stream (css (css-parser
                     (css-lexer
                      (babel-input-stream
                       (unistd-stream-open path :read t)))))
    (stream-read css)))

(defun run ()
  (simple-test))
