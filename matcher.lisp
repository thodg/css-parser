
(in-package :parse-css)

(defgeneric cb= (parser item &key start1))
(defgeneric match (parser item))
(defgeneric match-until (parser item))
(defgeneric match-option (parser function))
(defgeneric match-times (parser function min max))

(defmethod cb= ((p parser) (s string) &key (start1 0))
  (let ((cb (parser-cb p))
	(end (length s)))
    (when (<= end (- (fill-pointer cb) start1))
      (locally (declare (optimize (safety 0)))
	(labels ((at (m i)
		   (declare (type fixnum m i))
		   (cond ((= end i)
			  t)
			 ((= (aref cb m) (char-code (char s i)))
			  (at (the fixnum (1+ m)) (the fixnum (1+ i))))
			 (t
			  nil))))
	  (at start1 0))))))

(defmethod match ((p parser) (s string))
  (input-length p (length s))
  (when (cb= p s :start1 (parser-char-match-start p))
    (incf (parser-char-match-start p) (length s))))

(defmethod match ((p parser) (c fixnum))
  (when (= (the fixnum (parser-match-char p)) c)
    (incf (parser-char-match-start p))))

(defmethod match ((p parser) (c character))
  (match p (char-code c)))

(defmethod match-until ((p parser) (s string))
  (input-length p (length s))
  (labels ((maybe-eat ()
	     (or (match p s)
		 (and (not (match p -1))
		      (progn
			(input-char p)
			(incf (parser-char-match-start p))
			(maybe-eat))))))
    (maybe-eat)))

(defmethod match-option ((p parser) (f function))
  (or (funcall f p)
      (parser-char-match-start p)))

(defmacro match-not (p &body body)
  (let ((parser (gensym "PARSER-"))
	(match-start (gensym "MATCH-START-"))
	(result (gensym "RESULT-")))
    `(let* ((,parser ,p)
	    (,match-start (parser-char-match-start ,parser))
	    (,result (progn ,@body)))
       (cond ((or ,result
		  (match p -1))
	      (setf (parser-char-match-start ,parser) ,match-start)
	      nil)
	     (t
	      (incf (parser-char-match-start p)))))))

(defmacro match-sequence (p &body body)
  (let ((parser (gensym "PARSER-"))
	(match-start (gensym "MATCH-START-"))
	(result (gensym "RESULT-")))
    `(let* ((,parser ,p)
	    (,match-start (parser-char-match-start ,parser))
	    (,result (progn ,@body)))
       (cond (,result
	      ,result)
	     (t
	      (setf (parser-char-match-start ,parser) ,match-start)
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
		      (parser-char-match-start p)))))
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
		      (parser-char-match-start p)))))
      (match-min))))
