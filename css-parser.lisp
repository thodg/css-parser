
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

(defclass with-block (item)
  ((block :initarg :block
	  :initform nil
	  :accessor item-block
	  :type list)))

(defclass stylesheet (item) ())

(defclass toplevel-flag (item)
  ((toplevel :initarg :toplevel
	     :initform nil
	     :accessor item-toplevel
	     :type boolean)))
  
(defclass at-rule (named-item with-prelude with-block toplevel-flag) ())
(defclass qualified-rule (with-prelude with-block toplevel-flag) ())

(defclass css-declaration (named-item)
  ((value :initarg :value
	  :accessor item-value
	  :type list)
   (important-p :initarg :important
		:initform nil
		:accessor important-p
		:type boolean)))

(defclass component-value (item) ())

(defclass css-parser (parser)
  ((item :initform (make-instance 'stylesheet)
	 :accessor parser-item
	 :type item)))

;;  Parsing

(defgeneric parser-error (parser message))
(defgeneric assert-item (parser type))
(defgeneric parse-at-rule (parser &key toplevel))
(defgeneric parse-qualified-rule (parser &key toplevel))
(defgeneric parse-component-value* (parser))
(defgeneric parse-{}-block (parser))

(defmethod parser-error ((p parser) (message string))
  (error "CSS error ~S ~A line ~D character ~D"
	 message
	 (parser-input p)
	 (parser-input-line p)
	 (parser-input-character p)))

(defmethod assert-item ((p parser) (type symbol))
  (or (typep (parser-item p) type)
      (parser-error p (format nil "rule expected inside ~A" type))))

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
	(or (semicolon-token p)
	    (parse-{}-block p))
	(push item (item-at-rules parent))))))

(defmethod parse-rule-list ((p parser))
  (or (and (or (parse-at-rule p)
	       (parse-qualified-rule p)
	       (whitespace-token p))
	   (parse-rule-list p))
      t))

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
