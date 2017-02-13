
(in-package :parse-css)

(defclass item ()
  ((parent :initarg :parent
	   :accessor item-parent
	   :type (or null item))))

(defclass named-item (item)
  ((name :initarg :name
	 :accessor item-name
	 :type string)))

(defclass with-prelude (item)
  ((prelude :initarg :prelude
	    :initform nil
	    :accessor item-prelude
	    :type list)))

(defclass with-items (item)
  ((items :initarg :items
	  :initform nil
	  :accessor item-items
	  :type list)))

(defclass stylesheet (with-items) ())

(defclass toplevel-flag (item)
  ((toplevel :initarg :toplevel
	     :initform nil
	     :accessor item-toplevel
	     :type boolean)))

(defclass at-rule (named-item with-prelude with-items toplevel-flag) ())
(defclass qualified-rule (with-prelude with-items toplevel-flag) ())

(defclass css-declaration (named-item)
  ((values :initarg :value
	   :initform nil
	   :accessor item-value
	   :type list)
   (important-p :initarg :important
		:initform nil
		:accessor important-p
		:type boolean)))

(defclass component-value (item) ())

(defclass values-block (item)
  ((values :initarg :values
	   :accessor item-values
	   :type list)))

(defclass {}-block (values-block) ())
(defclass paren-block (values-block) ())
(defclass []-block (values-block) ())
(defclass function-block (values-block)
  ((function :initarg :function
	     :reader item-function
	     :type function-token)))

(defun make-item-buffer ()
  (make-array 64
	      :adjustable t
	      :fill-pointer 0))

(defmethod item-push-extend ((p parser) (item item))
  (vector-push-extend item (parser-ib p) 64))

(defmethod item-input ((p parser) (length fixnum))
  (unless (= 0 length)
    (item-push-extend p (consume-token p))
    (item-input p (1- length))))

(defmethod parser-match-item ((p parser))
  (item-input p 1)
  (aref (parser-ib p) (parser-item-match-start p)))

(defmethod item-match ((p parser) (type symbol))
  (let ((item (parser-match-item p)))
    (when (typep item type)
      (incf (parser-item-match-start p))
      item)))

(defmethod item-match ((p parser) (type list))
  (let ((item (parser-match-item p)))
    (when (typep item type)
      (incf (parser-item-match-start p))
      item)))

;;  Parsing

(defgeneric parser-error (parser message))
(defgeneric assert-item (parser type))
(defgeneric parse-at-rule (parser &key toplevel))
(defgeneric parse-qualified-rule (parser &key toplevel))
(defgeneric parse-component-value (parser))
(defgeneric parse-component-value* (parser))
(defgeneric parse-{}-block (parser))
(defgeneric parse-paren-block (parser))
(defgeneric parse-[]-block (parser))
(defgeneric parse-function-block (parser))
(defgeneric parse-preserved-token (parser))

(defmethod parser-error ((p parser) (message string))
  (error "CSS error ~A:~D:~D ~A"
	 (parser-input p)
	 (parser-input-line p)
	 (parser-input-character p)
	 message))

(defmethod assert-item ((p parser) (type symbol))
  (or (typep (parser-item p) type)
      (parser-error p (format nil "rule expected inside ~A" type))))

(defmethod pop-parser-item ((p parser))
  (setf (parser-item p) (item-parent (parser-item p))))

(defmethod parse-component-value ((p parser))
  (or (parse-function-block p)
      (parse-[]-block p)
      (parse-paren-block p)
      (parse-{}-block p)
      (parse-preserved-token p)))

(defmethod parse-component-value* ((p parser))
  (let ((value (parse-component-value p)))
    (when value
      (cons value (parse-component-value* p)))))

(defmethod parse-{}-block ((p parser))
  (when ({-token p)
    (let ((values (parse-component-value* p)))
      (if (}-token p)
	  (make-instance '{}-block
			 :values values)
	  (parser-error p "expected '}'")))))

(defmethod parse-paren-block ((p parser))
  (when (left-paren-token p)
    (let ((values (parse-component-value* p)))
      (if (right-paren-token p)
	  (make-instance 'paren-block
			 :values values)
	  (parser-error p "expected ')'")))))

(defmethod parse-[]-block ((p parser))
  (when ([-token p)
    (let ((values (parse-component-value* p)))
      (if (]-token p)
	  (make-instance '[]-block
			 :values values)
	  (parser-error p "expected ']'")))))

(defmethod parse-function-block ((p parser))
  (let ((fun (function-token p)))
    (when fun
      (let ((values (parse-component-value* p)))
	(if (right-paren-token p)
	    (make-instance 'function-block
			   :function fun
			   :values values)
	    (parser-error p "expected ')'"))))))

(defmethod parse-preserved-token ((p parser))
  (let ((token (consume-token p)))
    (if (typep token '(or function-token {-token left-paren-token [-token))
	(parser-error p (format nil "unexpected '~A'" (token-string token)))
	token)))

(defmethod parse-at-rule ((p parser) &key toplevel)
  (let ((at-keyword (at-keyword-token p)))
    (when at-keyword
      (let* ((parent (parser-item p))
	     (name (token-string (token-ident at-keyword)))
	     (prelude (parse-component-value* p))
	     (item (make-instance 'at-rule
				  :parent parent
				  :name name
				  :prelude prelude
				  :toplevel toplevel)))
	(setf (parser-item p) item)
	(cond ((or (semicolon-token p)
		   (parse-{}-block p))
	       (push item (item-items parent))
	       (pop-parser-item p)
	       item)
	      (t
	       (pop-parser-item p)
	       nil))))))

(defmethod parse-rule-list ((p parser))
  (if (or (parse-at-rule p)
	  (parse-qualified-rule p)
	  (whitespace-token p))
      (parse-rule-list p))
      t)

(defmethod parse-stylesheet ((p parser))
  (setf (parser-item p) (make-instance 'stylesheet :parent nil))
  (or (and (or (parse-at-rule p :toplevel t)
	       (parse-qualified-rule p :toplevel t)
	       (whitespace-token p)
	       (cdc-token p)
	       (cdc-token p))
	   (parse-stylesheet p))
      (eof-token p)
      (parser-error p "at top level")))
