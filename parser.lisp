
(in-package :parse-css)

(defvar *buffer-size* 64)

(defclass item ()
  ((tokens :initform #()
           :accessor item-tokens
           :type simple-vector)))

(defgeneric item-character (item))
(defgeneric item-line (item))
(defgeneric item-token (item n))

(defmethod item-token ((item item) (n integer))
  (declare (type fixnum n))
  (svref (item-tokens item) n))

(defmethod item-line ((item item))
  (token-stream:token-line (item-token item 0)))

(defmethod item-character ((item item))
  (token-stream:token-character (item-token item 0)))

(defun make-buffer ()
  (make-array `(,*buffer-size*)
	      :adjustable t
              :element-type '(or null item)
	      :fill-pointer 0
              :initial-element nil))

(defclass parser (super-stream input-stream)
  ((buffer :initform (make-buffer)
           :accessor parser-buffer
           :type (vector token))
   (eof-p :initform nil
          :accessor parser-eof-p
          :type boolean)
   (input-ended :initform nil
                :accessor parser-input-ended
                :type boolean)
   (match-start :initform 0
                :accessor parser-match-start
                :type fixnum)
   (stack :initform ()
          :accessor parser-stack
          :type list)))

(defmethod stream-element-type ((stream parser))
  'item)

;;  Input

(defmethod parser-push-extend ((pr parser) item)
  (vector-push-extend item (parser-buffer pr) 64))

(defmethod parser-token ((pr parser) (index integer))
  (declare (type fixnum index))
  (let ((buf (parser-buffer pr)))
    (declare (type (vector token) buf))
    (when (< index (fill-pointer buf))
      (aref buf index))))

(defmethod parser-input ((pr parser))
  (let ((in (stream-underlying-stream pr)))
    (multiple-value-bind (token state) (stream-read in)
      (ecase state
        ((nil) (parser-push-extend pr token)
         (values token nil))
        ((:eof) (setf (parser-input-ended pr) t)
         (values nil :eof))
        ((:non-blocking)
         (signal (make-condition 'non-blocking :stream pr)))))))

(defmethod parser-input-n ((pr parser) (n integer))
  (declare (type fixnum n))
  (loop
     (let ((length (- (the fixnum (fill-pointer (parser-buffer pr)))
                      (the fixnum (parser-match-start pr)))))
       (declare (type fixnum length))
       (when (parser-input-ended pr)
         (return))
       (unless (< length n)
         (return))
       (parser-input pr))))

(defmethod parser-match-token ((pr parser) (index integer))
  (declare (type fixnum index))
  (parser-input-n pr (the fixnum (1+ index)))
  (let ((buf (parser-buffer pr))
        (match-index (+ (the fixnum (parser-match-start pr))
                        index)))
    (declare (type vector buf)
             (type fixnum match-index))
    (aref (the (vector token) buf) match-index)))

;;  Matcher

(defmethod match ((pr parser) (type symbol))
  (let ((item (parser-match-token pr 0)))
    (when (subtypep (type-of item) (find-class type))
      (incf (the fixnum (parser-match-start pr)))
      item)))

(defmethod match-or ((pr parser) (types cons))
  (loop
     (when (endp types)
       (return))
     (let* ((type (pop types))
            (token (match pr type)))
       (when token
         (return token)))))

(defmethod match-option ((pr parser) (f function))
  (or (funcall f pr)
      (parser-match-start pr)))

(defmethod match-not ((pr parser) (f function))
  (let ((match-start (parser-match-start pr)))
    (cond ((or (funcall f pr)
               (parser-input-ended pr))
           (setf (parser-match-start pr) match-start)
           nil)
          (t
           (let ((token (parser-match-token pr 0)))
             (incf (the fixnum (parser-match-start pr)))
             token)))))

(defmacro match-sequence (parser &body body)
  (let ((pr (gensym "PR-"))
	(match-start (gensym "MATCH-START-"))
	(result (gensym "RESULT-")))
    `(let* ((,pr ,parser)
	    (,match-start (parser-match-start ,pr))
	    (,result (progn ,@body)))
       (cond (,result
	      ,result)
	     (t
	      (setf (parser-match-start ,pr) ,match-start)
	      nil)))))

(defmethod match-times ((pr parser) (f function) (min integer) (max integer))
  (declare (type fixnum min max))
  (match-sequence pr
    (let ((n 0))
      (loop
         (unless (< n max)
           (return (parser-match-start pr)))
         (unless (funcall f pr)
           (if (< n min)
               (return nil)
               (return (parser-match-start pr))))
         (incf n)))))

(defmethod match-times ((pr parser) (f function) (min integer) (max null))
  (declare (type fixnum min))
  (match-sequence pr
    (let ((n 0))
      (declare (type fixnum n))
      (loop
         (unless (funcall f pr)
           (if (< n min)
               (return nil)
               (return (parser-match-start pr))))
         (incf n)))))

(defmethod stream-read ((pr parser))
  (if (parser-eof-p pr)
      (values nil :eof)
      (handler-case (values (parser-parse pr) nil)
        (end-of-file () (values nil :eof))
        (non-blocking () (values nil :non-blocking)))))

;;  Item stack

(defmethod parser-push ((pr parser))
  (push (parser-match-start pr) (parser-stack pr)))

(defmethod parser-pop ((pr parser))
  (assert (parser-stack pr))
  (let* ((buffer (the (vector token) (parser-buffer pr)))
	 (fp (fill-pointer buffer))
	 (start (pop (parser-stack pr)))
	 (match-start (parser-match-start pr))
         (tokens (subseq buffer start match-start))
         (item (make-instance 'item :tokens tokens)))
    (when (endp (parser-stack pr))
      (replace buffer buffer :start2 match-start :end2 fp)
      (setf (parser-match-start pr) 0
	    (fill-pointer (parser-buffer pr)) (- fp match-start)))
    item))

(defmethod parser-discard ((pr parser))
  (assert (parser-stack pr))
  (let* ((buffer (the (vector token) (parser-buffer pr)))
	 (fp (fill-pointer buffer))
	 (match-start (parser-match-start pr)))
    (pop (parser-stack pr))
    (when (endp (parser-stack pr))
      (replace buffer buffer :start2 match-start :end2 fp)
      (setf (parser-match-start pr) 0
	    (fill-pointer buffer) (- fp match-start)))
    nil))
