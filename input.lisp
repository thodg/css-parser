
(in-package :parse-css)

(defgeneric cb-string (cb start end))
(defgeneric cb-push-extend (parser character))
(defgeneric input-char (parser))
(defgeneric input-length (parser length))
(defgeneric parser-match-char (parser &optional char-index))

(defun make-character-buffer (&optional string (start 0))
  (let* ((length (when string (- (length string) start)))
	 (cb (make-array (if string
			     (* 64 (ceiling length 64))
			     64)
			 :element-type 'fixnum
			 :adjustable t
			 :fill-pointer 0)))
    (when string
      (setf (fill-pointer cb) length)
      (replace cb string :start2 start))
    cb))

(defmethod cb-string ((cb array) (start fixnum) (end fixnum))
  (let* ((length (- end start))
	 (s (make-string length)))
    (labels ((at (i j)
	       (let ((cb-char (aref cb i)))
		 (unless (or (= length j)
			     (= cb-char -1))
		   (setf (char s j) (code-char cb-char))
		   (at (1+ i) (1+ j))))))
      (at start 0))))
    
(defmethod cb-push-extend ((p parser) (c fixnum))
  (let ((cb (parser-cb p)))
    (let* ((fill-pointer (fill-pointer cb))
	   (new-fill-pointer (1+ fill-pointer)))
      (if (= fill-pointer (array-dimension cb 0))
	  (setf (parser-cb p) (adjust-array cb (+ fill-pointer 64)
					    :fill-pointer new-fill-pointer))
	  (setf (fill-pointer cb) new-fill-pointer))
      (locally (declare (optimize (safety 0)))
	(setf (aref cb fill-pointer) c))
      fill-pointer)))

(defun read-char-code (stream)
  (let ((c (read-char stream nil nil)))
    (if c (char-code c) -1)))

(defmethod input-char ((p parser))
  (let* ((in (parser-input p))
	 (c (read-char-code in))
	 (pos (cb-push-extend p c)))
    (cond ((or (and (= #x000A c)
		    (not (and (< 0 pos)
			      (= #x000D (aref (parser-cb p) (1- pos))))))
	       (= #x000D c)
	       (= #x000C c))
	   (setf (parser-input-character p) 0)
	   (incf (parser-input-line p)))
	  (t
	   (incf (parser-input-character p))))
    c))

(defmethod input-length ((p parser) (length fixnum))
  (when (< (- (fill-pointer (parser-cb p))
	      (parser-char-match-start p))
	   length)
    (input-char p)
    (input-length p length)))

(defmethod parser-match-char ((p parser) &optional (index 0))
  (input-length p (1+ index))
  (aref (parser-cb p) (+ (parser-char-match-start p) index)))
