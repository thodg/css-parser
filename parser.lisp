
(in-package :common-lisp-user)

(defpackage :parse-css
  (:use :common-lisp)
  (:export #:parser))

(in-package :parse-css)

;;  Input buffers

(defun make-input-buffer (&optional string (start 0))
  (let* ((length (when string (- (length string) start)))
	 (ib (make-array (if string
			     (* 64 (ceiling length 64))
			     64)
			 :element-type 'character
			 :adjustable t
			 :fill-pointer 0)))
    (when string
      (setf (fill-pointer ib) length)
      (replace ib string :start2 start))
    ib))

;;  Parser

(defclass token ()
  ((string :initarg :string
	   :accessor token-string
	   :type string)
   (line :initarg :line
	 :initform 0
	 :accessor token-line
	 :type fixnum)
   (character :initarg :character
	      :initform 0
	      :accessor token-character
	      :type fixnum)))

(defclass parser-token (token)
  ((start :initarg :start
	  :accessor token-start
	  :type fixnum)))

(defclass parser ()
  ((input :initarg :stream
	   :reader parser-input
	   :type stream)
   (input-line :initarg :input-line
	       :initform 0
	       :accessor parser-input-line
	       :type fixnum)
   (input-character :initarg :input-character
		    :initform -1
		    :accessor parser-input-character
		    :type fixnum)
   (ib :initform (make-input-buffer)
	  :accessor parser-ib
	  :type array)
   (match-start :initform 0
		:accessor parser-match-start
		:type fixnum)
   (token-stack :initform ()
		:accessor parser-token-stack
		:type list)))

(defgeneric push-token (parser))
(defgeneric pop-token (parser))
(defgeneric make-token (parser class &rest initargs))
(defgeneric discard-token (parser))
(defgeneric ib-push-extend (parser character))
(defgeneric input-char (parser))
(defgeneric input-length (parser length))
(defgeneric parser-match-char (parser &optional char-index))
(defgeneric match (parser item))
(defgeneric match-until (parser item))
(defgeneric match-option (parser function))
(defgeneric match-times (parser function min max))
(defgeneric match-comment (parser))
(defgeneric match-newline (parser))
(defgeneric match-whitespace (parser))
(defgeneric match-hex-digit (parser))
(defgeneric match-escape (parser))
(defgeneric whitespace-token (parser))
(defgeneric match-ws* (parser))
(defgeneric match-ident-char (parser))
(defgeneric match-ident-char* (parser))
(defgeneric ident-token (parser))
(defgeneric function-token (parser))
(defgeneric at-keyword-token (parser))
(defgeneric match-string-char (parser end-char))
(defgeneric match-string (parser end-char))
(defgeneric string-token (parser))
(defgeneric match-non-printable (parser))
(defgeneric match-url-unquoted-char (parser))
(defgeneric match-url-unquoted (parser))
(defgeneric url-token (parser))
(defgeneric match-digit (parser))
(defgeneric match-digit+ (parser))
(defgeneric number-token (parser))
(defgeneric dimension-token (parser))
(defgeneric percentage-token (parser))
(defgeneric unicode-range-token (parser))
(defgeneric include-match-token (parser))
(defgeneric dash-match-token (parser))
(defgeneric prefix-match-token (parser))
(defgeneric suffix-match-token (parser))
(defgeneric substring-match-token (parser))
(defgeneric column-token (parser))
(defgeneric cdo-token (parser))
(defgeneric cdc-token (parser))

;;  Token stack

(defmethod push-token ((p parser))
  (let ((token (make-instance 'parser-token
			      :start (parser-match-start p)
			      :line (parser-input-line p)
			      :character (parser-input-character p))))
    (push token (parser-token-stack p))))

(defmethod pop-token ((p parser))
  (assert (parser-token-stack p))
  (let* ((ib (parser-ib p))
	 (fill-pointer (fill-pointer ib))
	 (token (pop (parser-token-stack p)))
	 (match-start (parser-match-start p)))
    (setf (token-string p) (subseq ib
				   (token-start token)
				   match-start))
    (when (endp (parser-token-stack p))
      (replace ib ib :start2 match-start :end2 fill-pointer)
      (setf (parser-match-start p) 0
	    (fill-pointer (parser-ib p)) (- fill-pointer match-start)))
    token))

(defmethod make-token ((p parser) (class symbol) &rest initargs)
  (let ((pt (pop-token p)))
    (apply #'make-instance 
	   :string (token-string pt)
	   :line (token-line pt)
	   :character (token-character pt)
	   initargs)))

(defmethod discard-token ((p parser))
  (pop-token p)
  nil)

;;  Parser input

(defmethod ib-push-extend ((p parser) (c character))
  (let ((ib (parser-ib p)))
    (let* ((fill-pointer (fill-pointer ib))
	   (new-fill-pointer (1+ fill-pointer)))
      (if (= fill-pointer (array-dimension ib 0))
	  (setf (parser-ib p) (adjust-array ib (+ fill-pointer 64)
					    :fill-pointer new-fill-pointer))
	  (setf (fill-pointer ib) new-fill-pointer))
      (locally (declare (optimize (safety 0)))
	(setf (aref ib fill-pointer) c))
      fill-pointer)))

(defmethod input-char ((p parser))
  (let* ((in (parser-input p))
	 (c (read-char in nil))
	 (pos (ib-push-extend p c)))
    (cond ((or (and (char= #\Newline c)
		    (not (and (< 0 pos)
			      (char= #\Return
				     (char (parser-ib p) (1- pos))))))
	       (char= #\Return c))
	   (setf (parser-input-character p) 0)
	   (incf (parser-input-line p)))
	  (t
	   (incf (parser-input-character p))))
    c))

(defmethod input-length ((p parser) (length fixnum))
  (when (< (- (fill-pointer (parser-ib p))
	      (parser-match-start p))
	   length)
    (input-char p)
    (input-length p length)))

(defmethod parser-match-char ((p parser) &optional (char 0))
  (input-length p (1+ char))
  (char (parser-ib p) (+ (parser-match-start p) char)))

;;  Pattern matching

(defmethod match ((p parser) (s string))
  (input-length p (length s))
  (when (string= (parser-ib p) s :start1 (parser-match-start p))
    (incf (parser-match-start p) (length s))))

(defmethod match ((p parser) (c character))
  (when (char= (parser-match-char p) c)
    (incf (parser-match-start p))))

(defmethod match-until ((p parser) (s string))
  (input-length p (length s))
  (labels ((maybe-eat ()
	     (or (match p s)
		 (progn
		   (input-char p)
		   (incf (parser-match-start p))
		   (maybe-eat)))))
    (maybe-eat)))

(defmethod match-option ((p parser) (f function))
  (or (funcall f p)
      (parser-match-start p)))

(defmacro match-not (p &body body)
  (let ((parser (gensym "PARSER-"))
	(match-start (gensym "MATCH-START-"))
	(result (gensym "RESULT-")))
    `(let* ((,parser ,p)
	    (,match-start (parser-match-start ,parser))
	    (,result (progn ,@body)))
       (cond (,result
	      (setf (parser-match-start ,parser) ,match-start)
	      nil)
	     (t
	      (incf (parser-match-start p)))))))

(defmacro match-sequence (p &body body)
  (let ((parser (gensym "PARSER-"))
	(match-start (gensym "MATCH-START-"))
	(result (gensym "RESULT-")))
    `(let* ((,parser ,p)
	    (,match-start (parser-match-start ,parser))
	    (,result (progn ,@body)))
       (cond (,result
	      ,result)
	     (t
	      (setf (parser-match-start ,parser) ,match-start)
	      nil)))))

(defmethod match-times ((p parser) (f function) (min fixnum) (max fixnum))
  (match-sequence p
    (labels ((match-min ()
	       (cond ((= 0 min)
		      (match-max))
		     ((funcall f p)
		      (decf min)
		      (decf max)
		      (match-min))
		     (t
		      nil)))
	     (match-max ()
	       (cond ((and (< 0 max) (funcall f p))
		      (decf max)
		      (match-max))
		     (t
		      (parser-match-start p)))))
      (match-min))))

(defmethod match-times ((p parser) (f function) (min fixnum) (max null))
  (match-sequence p
    (labels ((match-min ()
	       (cond ((= 0 min)
		      (match-max))
		     ((funcall f p)
		      (decf min)
		      (match-min))
		     (t
		      nil)))
	     (match-max ()
	       (cond ((funcall f p)
		      (match-max))
		     (t
		      (parser-match-start p)))))
      (match-min))))

;;  Productions

(defmethod match-comment ((p parser))
  (when (match p "/*")
    (match-until p "*/")))

(defmethod match-newline ((p parser))
  (or (match p #\Newline)
      (match p (coerce '(#\Return #\Newline) 'string))
      (match p #\Return)
      (match p #\Linefeed)))

(defmethod match-whitespace ((p parser))
  (or (match p #\Space)
      (match p #\Tab)
      (match-newline p)))

(defmethod match-hex-digit ((p parser))
  (let ((c (parser-match-char p)))
    (when (or (char<= #\0 c #\9)
	      (char<= #\a c #\f)
	      (char<= #\A c #\F))
      (incf (parser-match-start p)))))
       
(defmethod match-escape ((p parser))
  (match-sequence p
    (and (match p #\\)
	 (or (when (match-times p #'match-hex-digit 1 6)
	       (match-whitespace p)
	       (parser-match-start p))
	     (when (not (match-newline p))
	       (parser-match-start p))))))

(defclass whitespace-token (token) ())

(defmethod whitespace-token ((p parser))
  (push-token p)
  (if (match-times p #'match-whitespace 1 nil)
      (make-token p 'whitespace-token)
      (discard-token p)))

(defmethod match-ws* ((p parser))
  (match-option p #'whitespace-token))

(defclass ident-token (token) ())

(defmethod match-ident-char ((p parser))
  (or (match-escape p)
      (let ((c (parser-match-char p)))
	(when (or (char<= #\a c #\z)
		  (char<= #\A c #\Z)
		  (char<= #\0 c #\9)
		  (char= #\_ c)
		  (char= #\- c)
		  (< #x007F (char-code c)))
	  (incf (parser-match-start p))))))

(defmethod match-ident-char* ((p parser))
  (match-times p #'match-ident-char 0 nil))

(defmethod ident-token ((p parser))
  (match-sequence p
    (push-token p)
    (match p #\-)
    (cond ((or (match-escape p)
	       (let ((c (parser-match-char p)))
		 (when (or (char<= #\a c #\z)
			   (char<= #\A c #\Z)
			   (char= #\_ c)
			   (< #x007F (char-code c)))
		   (incf (parser-match-start p)))))
	   (match-ident-char* p)
	   (make-token p 'ident-token))
	  (t
	   (discard-token p)))))

(defclass identified-token (token)
  ((ident :initarg :ident
	  :reader token-ident
	  :type ident-token)))

(defclass function-token (identified-token) ())

(defmethod function-token ((p parser))
  (match-sequence p
    (push-token p)
    (let ((ident (ident-token p)))
      (if (and ident (match p #\())
	  (make-token p 'function-token :ident ident)
	  (discard-token p)))))

(defclass at-keyword-token (identified-token) ())

(defmethod at-keyword-token ((p parser))
  (match-sequence p
    (push-token p)
    (if (match p #\@)
	(let ((ident (ident-token p)))
	  (if ident
	      (make-token p 'at-keyword-token :ident ident)
	      (discard-token p)))
	(discard-token p))))

(defclass hash-token (token) ())

(defmethod hash-token ((p parser))
  (match-sequence p
    (push-token p)
    (if (match p #\#)
	(and (match-ident-char* p)
	     (make-token p 'hash-token))
	(discard-token p))))

(defclass string-token (token) ())

(defgeneric string-token-string (string-token))

(defmethod string-token-string ((s string-token))
  (let ((string (token-string s)))
    (subseq string 1 (1- (length string)))))

(defmethod match-string-char ((p parser) (end-char character))
  (or (match-sequence p
	(and (match p #\\)
	     (match-newline p)))
      (match-escape p)
      (match-not p
	(or (match p end-char)
	    (match p #\\)
	    (match-newline p)))))

(defmethod match-string ((p parser) (end-char character))
  (match-sequence p
    (match p end-char)
    (match-times p (lambda (p) (match-string-char p end-char)) 0 nil)
    (match p end-char)))

(defmethod string-token ((p parser))
  (push-token p)
  (if (or (match-string p #\")
	  (match-string p #\'))
      (make-token p 'string-token)))

(defclass url-token (identified-token)
  ((url :initarg :url
	:reader token-url
	:type token)))

(defmethod match-non-printable ((p parser))
  (let ((d (char-code (parser-match-char p))))
    (when (or (<= #x0000 d #x001F)
	      (=  #x007F d)
	      (<= #x0080 d #x009F))
      (incf (parser-match-start p)))))

(defmethod match-url-unquoted-char ((p parser))
  (or (match-escape p)
      (match-not p
	(or (match p #\")
	    (match p #\')
	    (match p #\()
	    (match p #\))
	    (match p #\\)
	    (match-whitespace p)
	    (match-non-printable p)))))

(defmethod match-url-unquoted ((p parser))
  (push-token p)
  (if (match-times p #'match-url-unquoted-char 1 nil)
      (make-token p 'token)
      (discard-token p)))

(defmethod url-token ((p parser))
  (push-token p)
  (or (match-sequence p
	(let ((ident (ident-token p)))
	  (and (string= "url" (token-string ident))
	       (match p #\()
	       (match-ws* p)
	       (let ((url (or (string-token p)
			      (match-url-unquoted p))))
		 (match-ws* p)
		 (when (match p #\))
		   (make-token p (url-token :ident ident :url url)))))))
      (discard-token p)))

(defclass number-token (token) ())

(defmethod match-digit ((p parser))
  (let ((c (parser-match-char p)))
    (when (char<= #\0 c #\9)
      (incf (parser-match-start p)))))

(defmethod match-digit+ ((p parser))
  (match-times p #'match-digit 1 nil))

(defmethod number-token ((p parser))
  (push-token p)
  (if (match-sequence p
	(and (or (match p #\-)
		 (match p #\+)
		 t)
	     (or (match-sequence p
		   (and (match p #\.)
			(match-digit+ p)))
		 (match-sequence p
		   (match-digit+ p)
		   (match p #\.)
		   (match-digit+ p))
		 (match-digit+ p))
	     (or (match-sequence p
		   (and (or (match p #\E)
			    (match p #\e))
			(or (match p #\-)
			    (match p #\+)
			    t)
			(match-digit+ p)))
		 (parser-match-start p))))
      (make-token p 'number-token)
      (discard-token p)))

(defclass numbered-token (token)
  ((number :initarg :number
	   :reader token-number
	   :type number-token)))

(defclass dimension-token (identified-token numbered-token) ())

(defmethod dimension-token ((p parser))
  (push-token p)
  (or (match-sequence p
	(let ((number (number-token p)))
	  (when number
	    (let ((ident (ident-token p)))
	      (when ident
		(make-token p 'dimension-token
			    :number number
			    :ident ident))))))
      (discard-token p)))

(defclass percentage-token (numbered-token) ())

(defmethod percentage-token ((p parser))
  (push-token p)
  (or (match-sequence p
	(let ((number (number-token p)))
	  (when number
	    (when (match p #\%)
	      (make-token p 'percentage-token
			  :number number)))))
      (discard-token p)))

(defclass unicode-range-token (token) ())

(defmethod unicode-range-token ((p parser))
  (push-token p)
  (or (match-sequence p
	(and (or (match p #\u)
		 (match p #\U))
	     (match p #\+)
	     (or (match-sequence p
		   (and (match-times p #'match-hex-digit 1 6)
			(match p #\-)
			(match-times p #'match-hex-digit 1 6)))
		 (match-sequence p
		   (let ((start (parser-match-start p)))
		     (and (match-times p #'match-hex-digit 0 5)
			  (let ((digits (- (parser-match-start p) start)))
			    (match-times p (lambda (p) (match p #\?))
					 1 (- 6 digits))))))
		 (match-times p #'match-hex-digit 1 6))
	     (make-token p 'unicode-range-token)))
      (discard-token p)))

(defclass include-match-token (token) ())

(defmethod include-match-token ((p parser))
  (push-token p)
  (if (match p "~=")
      (make-token p 'include-match-token)
      (discard-token p)))

(defclass dash-match-token (token) ())

(defmethod dash-match-token ((p parser))
  (push-token p)
  (if (match p "|=")
      (make-token p 'dash-match-token)
      (discard-token p)))

(defclass prefix-match-token (token) ())

(defmethod prefix-match-token ((p parser))
  (push-token p)
  (if (match p "^=")
      (make-token p 'prefix-match-token)
      (discard-token p)))

(defclass suffix-match-token (token) ())

(defmethod suffix-match-token ((p parser))
  (push-token p)
  (if (match p "$=")
      (make-token p 'suffix-match-token)
      (discard-token p)))

(defclass substring-match-token (token) ())

(defmethod substring-match-token ((p parser))
  (push-token p)
  (if (match p "*=")
      (make-token p 'substring-match-token)
      (discard-token p)))

(defclass column-token (token) ())

(defmethod column-token ((p parser))
  (push-token p)
  (if (match p "||")
      (make-token p 'column-token)
      (discard-token p)))

(defclass cdo-token (token) ())

(defmethod cdo-token ((p parser))
  (push-token p)
  (if (match p "<!--")
      (make-token p 'cdo-token)
      (discard-token p)))

(defclass cdc-token (token) ())

(defmethod cdc-token ((p parser))
  (push-token p)
  (if (match p "-->")
      (make-token p 'cdc-token)
      (discard-token p)))
