
(in-package :parse-css)

(defclass css-item (item)
  ((parent :initarg :parent
	   :accessor item-parent
	   :type (or null item))))

(defclass named-item (css-item)
  ((name :initarg :name
	 :accessor item-name
	 :type string)))

(defclass with-prelude (css-item)
  ((prelude :initarg :prelude
	    :initform nil
	    :accessor item-prelude
	    :type list)))

(defclass with-items (css-item)
  ((items :initarg :items
	  :initform nil
	  :accessor item-items
	  :type list)
   (items-end :initform nil
              :accessor item-items-end
              :type list)))

(defgeneric item-append (with-items item))

(defmethod item-append ((parent with-items) (item item))
  (let ((end (list item)))
    (cond ((endp (item-items parent))
           (setf (item-items parent) end
                 (item-items-end parent) end))
          (t
           (setf (cdr (item-items-end parent)) end
                 (item-items-end parent) end))))
  item)

(defclass stylesheet (with-items) ())

(defclass toplevel-flag (css-item)
  ((toplevel :initarg :toplevel
	     :initform nil
	     :accessor item-toplevel
	     :type boolean)))

(defclass at-rule (named-item with-prelude with-items toplevel-flag) ())
(defclass qualified-rule (with-prelude with-items toplevel-flag) ())

(defclass values-block (css-item)
  ((values :initarg :values
           :initform nil
	   :accessor item-values
	   :type list)))

(defclass css-declaration (named-item values-block)
  ((important-p :initarg :important
		:initform nil
		:accessor important-p
		:type boolean)))

(defclass {}-block (values-block) ())
(defclass paren-block (values-block) ())
(defclass []-block (values-block) ())
(defclass function-block (values-block)
  ((function :initarg :function
	     :reader item-function
	     :type function-token)))

(defclass css-parser (parser)
  ((item :initform (make-instance 'stylesheet)
         :accessor parser-item
         :type css-item)))

;;  Parsing

(defgeneric parser-error (parser &rest message))
(defgeneric parse-at-rule (parser &key toplevel))
(defgeneric parse-qualified-rule (parser &key toplevel))
(defgeneric parse-declaration (parser))
(defgeneric parse-component-value (parser))
(defgeneric parse-component-value* (parser))
(defgeneric parse-{}-block (parser))
(defgeneric parse-paren-block (parser))
(defgeneric parse-[]-block (parser))
(defgeneric parse-function-block (parser))
(defgeneric parse-preserved-token (parser))

(defmethod parser-error ((pr parser) &rest message)
  (let ((token (parser-token pr 0)))
    (if token
        (error "CSS error ~A:~A ~A"
               (token-line token)
               (token-character token)
               (str message))
        (error "CSS error ~A" (str message)))))

(defmethod parse-component-value ((pr parser))
  (or (parse-function-block pr)
      (parse-[]-block pr)
      (parse-paren-block pr)
      (parse-declaration pr)
      (parse-preserved-token pr)))

(defmethod parse-component-value* ((pr parser))
  (let ((values ()))
    (loop (or (match pr 'whitespace-token)
              (let ((value (parse-component-value pr)))
                (when value
                  (push value values)))
              (return)))
    (nreverse values)))

(defmethod parse-{}-block ((pr parser))
  (match-sequence pr
    (when (match pr '{-token)
      (let ((values (parse-component-value* pr)))
        (when values
          (if (match pr '}-token)
              (setf (item-items (parser-item pr)) values)
              (parser-error pr "expected '}'")))))))

(defmethod parse-paren-block ((pr parser))
  (match-sequence pr
    (when (match pr 'left-paren-token)
      (let ((values (parse-component-value* pr)))
        (if (match pr 'right-paren-token)
            (make-instance 'paren-block :values values)
            (parser-error pr "expected ')'"))))))

(defmethod parse-[]-block ((pr parser))
  (match-sequence pr
    (when (match pr '[-token)
      (let ((values (parse-component-value* pr)))
        (if (match pr ']-token)
            (make-instance '[]-block :values values)
            (parser-error pr "expected ']'"))))))

(defmethod parse-function-block ((pr parser))
  (match-sequence pr
    (let ((fun (match pr 'function-token)))
      (when fun
        (let ((values (parse-component-value* pr)))
          (if (match pr 'right-paren-token)
              (make-instance 'function-block
                             :function fun
                             :values values)
              (parser-error pr "expected ')'")))))))

(defmethod parse-preserved-token ((pr parser))
  (match-not pr (lambda (pr)
                  (match-or pr '({-token }-token
                                 [-token ]-token
                                 left-paren-token right-paren-token
                                 eof-token)))))

(defmethod parse-declaration ((pr parser))
  (match-sequence pr
    (match pr 'whitespace-token)
    (let ((name (parse-preserved-token pr)))
      (when name
        (let ((prop (make-instance 'css-declaration
                                   :name (token-string name))))
          (trace match)
          (unwind-protect
               (progn
                 (match pr 'whitespace-token)
                 (when (match pr 'colon-token)
                   (loop
                      (match pr 'whitespace-token)
                      (when (or (match pr 'semicolon-token)
                                (typep (parser-match-token pr 0) '}-token))
                        (return prop))
                      (let ((value (parse-preserved-token pr)))
                        (when value
                          (push value (item-values prop)))))))
            (untrace match)))))))
  
(defmethod parse-at-rule ((pr parser) &key toplevel)
  (parser-push pr)
  (let ((at-keyword (match pr 'at-keyword-token)))
    (when at-keyword
      (let* ((parent (parser-item pr))
             (name (token-string (token-ident at-keyword)))
             (prelude (parse-component-value* pr))
             (item (make-instance 'at-rule
                                  :parent parent
                                  :name name
                                  :prelude prelude
                                  :toplevel toplevel)))
        (setf (parser-item pr) item)
	(cond ((or (semicolon-token pr)
                   (parse-{}-block pr))
               (item-append parent item)
               (setf (parser-item pr) parent)
               item)
	      (t
	       (parser-pop pr)
	       nil))))))

(defmethod parse-rule-list ((pr parser))
  (if (or (parse-at-rule pr)
	  (parse-qualified-rule pr)
	  (match pr 'whitespace-token))
      (parse-rule-list pr))
      t)

(defmethod parse-qualified-rule ((pr parser) &key toplevel)
  (match-sequence pr
    (let* ((parent (parser-item pr))
           (prelude (parse-component-value* pr))
           (item (make-instance 'qualified-rule
                                :prelude prelude
                                :parent parent
                                :toplevel toplevel)))
      (setf (parser-item pr) item)
      (let ((block (parse-{}-block pr)))
        (when block
          (setf (item-items item) block)
          (item-append parent item))
        (setf (parser-item pr) parent)
        (when block
          item)))))

(defmethod parse-stylesheet ((pr parser))
  (let ((stylesheet (make-instance 'stylesheet :parent nil)))
    (setf (parser-item pr) stylesheet)
    (loop
       (unless (or (parse-at-rule pr :toplevel t)
                   (parse-qualified-rule pr :toplevel t)
                   (match pr 'whitespace-token)
                   (match pr 'cdo-token)
                   (match pr 'cdc-token))
         (return)))
    (unless (match pr 'eof-token)
      (parser-error pr "at top level"))
    stylesheet))

(defmethod parser-parse ((pr parser))
  (parse-stylesheet pr))

(defun css-parser (stream)
  (make-instance 'css-parser :stream stream))

(trace
 item-append
 ;match-not
 ;match-or
 parse-stylesheet
 parse-qualified-rule
 parse-rule-list
 parse-at-rule
 parse-preserved-token
 ;parse-function-block
 ;parse-[]-block
 ;parse-paren-block
 parse-{}-block
 parse-component-values*
 parse-component-value
 parse-declaration
 )
