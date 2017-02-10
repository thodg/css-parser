
(in-package :parse-css)

(defgeneric ib-string (ib start end))
(defgeneric ib-push-extend (parser character))
(defgeneric input-char (parser))
(defgeneric input-length (parser length))
(defgeneric parser-match-char (parser &optional char-index))

(defun make-input-buffer (&optional string (start 0))
  (let* ((length (when string (- (length string) start)))
	 (ib (make-array (if string
			     (* 64 (ceiling length 64))
			     64)
			 :element-type 'fixnum
			 :adjustable t
			 :fill-pointer 0)))
    (when string
      (setf (fill-pointer ib) length)
      (replace ib string :start2 start))
    ib))

(defmethod ib-string ((ib array) (start fixnum) (end fixnum))
  (let* ((length (- end start))
	 (s (make-string length)))
    (labels ((at (i j)
	       (let ((ib-char (aref ib i)))
		 (unless (or (= length j)
			     (= ib-char -1))
		   (setf (char s j) (code-char ib-char))
		   (at (1+ i) (1+ j))))))
      (at start 0))))
    
(defmethod ib-push-extend ((p parser) (c fixnum))
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

(defun read-char-code (stream)
  (let ((c (read-char stream nil nil)))
    (if c (char-code c) -1)))

(defmethod input-char ((p parser))
  (let* ((in (parser-input p))
	 (c (read-char-code in))
	 (pos (ib-push-extend p c)))
    (cond ((or (and (= #x000A c)
		    (not (and (< 0 pos)
			      (= #x000D (aref (parser-ib p) (1- pos))))))
	       (= #x000D c)
	       (= #x000C c))
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

(defmethod parser-match-char ((p parser) &optional (index 0))
  (input-length p (1+ index))
  (aref (parser-ib p) (+ (parser-match-start p) index)))
