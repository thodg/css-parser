
(in-package :parse-css)

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

(defclass identified-token (token)
  ((ident :initarg :ident
	  :reader token-ident
	  :type ident-token)))

(defclass comment-token (token) ())
(defclass whitespace-token (token) ())
(defclass ident-token (token) ())
(defclass function-token (identified-token) ())
(defclass at-keyword-token (identified-token) ())
(defclass hash-token (token) ())
(defclass string-token (token) ())

(defclass url-token (identified-token)
  ((url :initarg :url
	:reader token-url
	:type token)))

(defclass number-token (token) ())

(defclass numbered-token (token)
  ((number :initarg :number
	   :reader token-number
	   :type number-token)))

(defclass dimension-token (identified-token numbered-token) ())
(defclass percentage-token (numbered-token) ())
(defclass unicode-range-token (token) ())
(defclass include-match-token (token) ())
(defclass dash-match-token (token) ())
(defclass prefix-match-token (token) ())
(defclass suffix-match-token (token) ())
(defclass substring-match-token (token) ())
(defclass column-token (token) ())
(defclass cdo-token (token) ())
(defclass cdc-token (token) ())
(defclass left-paren-token (token) ())
(defclass right-paren-token (token) ())
(defclass comma-token (token) ())
(defclass colon-token (token) ())
(defclass semicolon-token (token) ())
(defclass [-token (token) ())
(defclass ]-token (token) ())
(defclass {-token (token) ())
(defclass }-token (token) ())
(defclass eof-token (token) ())
(defclass delim-token (token) ())

(defmethod comment-token ((p parser))
  (push-token p)
  (if (match p "/*")
      (progn (match-until p "*/")
	     (make-token p 'comment-token))
      (discard-token p)))

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
  (let ((c (the fixnum (parser-match-char p))))
    (when (or (<= (char-code #\0) c (char-code #\9))
	      (<= (char-code #\a) c (char-code #\f))
	      (<= (char-code #\A) c (char-code #\F)))
      (incf (parser-match-start p)))))
       
(defmethod match-escape ((p parser))
  (match-sequence p
    (and (match p #\\)
	 (or (when (match-times p #'match-hex-digit 1 6)
	       (match-whitespace p)
	       (parser-match-start p))
	     (when (not (match-newline p))
	       (parser-match-start p))))))

(defmethod whitespace-token ((p parser))
  (push-token p)
  (if (match-times p #'match-whitespace 1 nil)
      (make-token p 'whitespace-token)
      (discard-token p)))

(defmethod match-ws* ((p parser))
  (match-option p #'whitespace-token))

(defmethod match-ident-char ((p parser))
  (or (match-escape p)
      (let ((c (the fixnum (parser-match-char p))))
	(when (or (<= (char-code #\a) c (char-code #\z))
		  (<= (char-code #\A) c (char-code #\Z))
		  (<= (char-code #\0) c (char-code #\9))
		  (=  (char-code #\_) c)
		  (=  (char-code #\-) c)
		  (<  #x007F c))
	  (incf (parser-match-start p))))))

(defmethod match-ident-char* ((p parser))
  (match-times p #'match-ident-char 0 nil))

(defmethod ident-token ((p parser))
  (match-sequence p
    (push-token p)
    (match p #\-)
    (cond ((or (match-escape p)
	       (let ((c (the fixnum (parser-match-char p))))
		 (when (or (<= (char-code #\a) c (char-code #\z))
			   (<= (char-code #\A) c (char-code #\Z))
			   (=  (char-code #\_) c)
			   (<  #x007F c))
		   (incf (parser-match-start p)))))
	   (match-ident-char* p)
	   (make-token p 'ident-token))
	  (t
	   (discard-token p)))))

(defmethod function-token ((p parser))
  (match-sequence p
    (push-token p)
    (let ((ident (ident-token p)))
      (if (and ident (match p #\())
	  (make-token p 'function-token :ident ident)
	  (discard-token p)))))

(defmethod at-keyword-token ((p parser))
  (match-sequence p
    (push-token p)
    (if (match p #\@)
	(let ((ident (ident-token p)))
	  (if ident
	      (make-token p 'at-keyword-token :ident ident)
	      (discard-token p)))
	(discard-token p))))

(defmethod hash-token ((p parser))
  (match-sequence p
    (push-token p)
    (if (match p #\#)
	(and (match-ident-char* p)
	     (make-token p 'hash-token))
	(discard-token p))))

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

(defmethod match-non-printable ((p parser))
  (let ((c (the fixnum (parser-match-char p))))
    (when (or (<= #x0000 c #x0008)
	      (=  #x000B c)
	      (<= #x000E c #x001F)
	      (=  #x007F c))
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

(defmethod match-digit ((p parser))
  (let ((c (the fixnum (parser-match-char p))))
    (when (<= (char-code #\0) c (char-code #\9))
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

(defmethod percentage-token ((p parser))
  (push-token p)
  (or (match-sequence p
	(let ((number (number-token p)))
	  (when number
	    (when (match p #\%)
	      (make-token p 'percentage-token
			  :number number)))))
      (discard-token p)))

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

(defmethod include-match-token ((p parser))
  (push-token p)
  (if (match p "~=")
      (make-token p 'include-match-token)
      (discard-token p)))

(defmethod dash-match-token ((p parser))
  (push-token p)
  (if (match p "|=")
      (make-token p 'dash-match-token)
      (discard-token p)))

(defmethod prefix-match-token ((p parser))
  (push-token p)
  (if (match p "^=")
      (make-token p 'prefix-match-token)
      (discard-token p)))

(defmethod suffix-match-token ((p parser))
  (push-token p)
  (if (match p "$=")
      (make-token p 'suffix-match-token)
      (discard-token p)))

(defmethod substring-match-token ((p parser))
  (push-token p)
  (if (match p "*=")
      (make-token p 'substring-match-token)
      (discard-token p)))

(defmethod column-token ((p parser))
  (push-token p)
  (if (match p "||")
      (make-token p 'column-token)
      (discard-token p)))

(defmethod cdo-token ((p parser))
  (push-token p)
  (if (match p "<!--")
      (make-token p 'cdo-token)
      (discard-token p)))

(defmethod cdc-token ((p parser))
  (push-token p)
  (if (match p "-->")
      (make-token p 'cdc-token)
      (discard-token p)))

(defmethod left-paren-token ((p parser))
  (push-token p)
  (if (match p #\()
      (make-token p 'left-paren-token)
      (discard-token p)))

(defmethod right-paren-token ((p parser))
  (push-token p)
  (if (match p #\))
      (make-token p 'right-paren-token)
      (discard-token p)))

(defmethod comma-token ((p parser))
  (push-token p)
  (if (match p #\,)
      (make-token p 'comma-token)
      (discard-token p)))

(defmethod colon-token ((p parser))
  (push-token p)
  (if (match p #\:)
      (make-token p 'colon-token)
      (discard-token p)))

(defmethod semicolon-token ((p parser))
  (push-token p)
  (if (match p #\;)
      (make-token p 'semicolon-token)
      (discard-token p)))

(defmethod [-token ((p parser))
  (push-token p)
  (if (match p #\[)
      (make-token p '[-token)
      (discard-token p)))

(defmethod ]-token ((p parser))
  (push-token p)
  (if (match p #\])
      (make-token p ']-token)
      (discard-token p)))

(defmethod {-token ((p parser))
  (push-token p)
  (if (match p #\{)
      (make-token p '{-token)
      (discard-token p)))

(defmethod }-token ((p parser))
  (push-token p)
  (if (match p #\})
      (make-token p '}-token)
      (discard-token p)))

(defmethod eof-token ((p parser))
  (push-token p)
  (if (match p -1)
      (make-token p 'eof-token)
      (discard-token p)))

(defmethod delim-token ((p parser))
  (push-token p)
  (input-length p 1)
  (incf (parser-match-start p))
  (make-token p 'delim-token))

;;  Tokenizer

(defmethod consume-token ((p parser))
  (or (whitespace-token p)
      (string-token p)
      (hash-token p)
      (suffix-match-token p)
      (left-paren-token p)
      (right-paren-token p)
      (substring-match-token p)
      (number-token p)
      (comma-token p)
      (cdc-token p)
      (comment-token p)
      (colon-token p)
      (semicolon-token p)
      (cdo-token p)
      (at-keyword-token p)
      ([-token p)
      (]-token p)
      (prefix-match-token p)
      ({-token p)
      (}-token p)
      (unicode-range-token p)
      (ident-token p)
      (dash-match-token p)
      (include-match-token p)
      (eof-token p)
      (delim-token p)))

;;  Lexer

(defun lex-stream (stream)
  (let ((p (make-instance 'parser :stream stream))
	(document nil))
    (labels ((push-tokens ()
	       (let ((token (consume-token p)))
		 (push token document)
		 (unless (typep token 'eof-token)
		   (push-tokens)))))
      (push-tokens))
    (nreverse document)))

(defun lex-string (string)
  (with-input-from-string (stream string)
    (lex-stream stream)))

(defun lex-file (filespec)
  (with-open-file (stream filespec
			  :element-type 'character
			  :external-format :utf-8)
    (lex-stream stream)))

;;  Tests

#+test
(lex-string "/* hello world */ body { color: #ff0000; }")
