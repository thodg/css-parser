
(in-package :parse-css)

(defgeneric push-token (parser))
(defgeneric pop-token (parser))
(defgeneric make-token (parser class &rest initargs))
(defgeneric discard-token (parser))

(defmethod push-token ((p parser))
  (let ((token (make-instance 'parser-token
			      :start (parser-char-match-start p)
			      :line (parser-input-line p)
			      :character (parser-input-character p))))
    (push token (parser-token-stack p))))

(defmethod pop-token ((p parser))
  (assert (parser-token-stack p))
  (let* ((cb (parser-cb p))
	 (fill-pointer (fill-pointer cb))
	 (token (pop (parser-token-stack p)))
	 (match-start (parser-char-match-start p)))
    (setf (token-string token) (cb-string cb
					  (token-start token)
					  match-start))
    (when (endp (parser-token-stack p))
      (replace cb cb :start2 match-start :end2 fill-pointer)
      (setf (parser-char-match-start p) 0
	    (fill-pointer (parser-cb p)) (- fill-pointer match-start)))
    token))

(defmethod make-token ((p parser) (class symbol) &rest initargs)
  (let ((pt (pop-token p)))
    (apply #'make-instance class
	   :string (token-string pt)
	   :line (token-line pt)
	   :character (token-character pt)
	   initargs)))

(defmethod discard-token ((p parser))
  (pop-token p)
  nil)
