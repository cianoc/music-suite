(defpackage music-suite/patterns
  (:use #:cl #:music-suite/sequences #:music-suite/math)
  (:local-nicknames (:t :transducers) (:alex :alexandria) (:serap :serapeum))
  (:import-from :mgl-pax #:defsection )
  (:import-from :trivia #:match))
(in-package #:music-suite/patterns)

;; Used internally for typechecking.
(deftype maybe-fixnum () '(or null fixnum))

(define-condition end-of-stream ()
  ())

(defstruct pattern
  "The root class for all patterns. A pattern is a template for creating 
streams. You will initiate a pattern with it's parameters, and then you can create
multiple streams from that pattern, each of which are fully independent of each other"
  (length nil :type maybe-fixnum :read-only t))

(defstruct (lazy-stream)
  "This is the root class for all lazy-streams. Lazy-streams are created from a pattern.

Lazy-streams are created one time and return values until they are used up."
  (length nil :type maybe-fixnum :read-only t)
  (remaining nil :type maybe-fixnum))

(defgeneric make-stream (pattern)
  (:documentation "When passed a pattern, this will create a new stream.

This must be overriden by all subclasses of pattern"))

(defmethod make-stream (obj)
  obj)

;; (defgeneric init-stream (stream)
;;   (:documentation "Make stream should call this after constructing the struct"))

;; (defmethod init-stream ((stream lazy-stream))
;;   (setf (lazy-stream-remaining stream)
;;         (lazy-stream-length stream)))

(defgeneric next (lazy-stream)
  (:documentation "This will advance the stream and return the next value in the stream. If
the stream is finished then this will return nil.

If creating a new type of pattern you typically will not need to override this."))

(defmethod next (obj)
  obj)

(defgeneric peek (lazy-stream)
  (:documentation "This allows you to see the next value in the stream, without advancing
the stream. This is typically something that should only be used by other sequence objects"))

(defmethod peek (obj)
  obj)

(defgeneric skip (lazy-stream n)
  (:documentation "This allows you to see the next value in the stream, without advancing
the stream. This is typically something that should only be used by other sequence objects"))

(defmethod skip (obj n)
  (declare (ignore n))
  obj)

(defmethod skip ((stream lazy-stream) n)
  (loop repeat n
        do (next stream)))

(declaim (ftype (function (lazy-stream)) dec-remaining)
         (inline dec-remaining))
(defun dec-remaining (stream)
  (when (and (active-p stream) (lazy-stream-remaining stream))
    (decf (lazy-stream-remaining stream))))

(declaim (ftype (function (lazy-stream)) remaining-p)
         (inline remaining-p))
(defun remaining-p (stream)
  (let ((remaining (lazy-stream-remaining stream)))
    (if remaining 
        (> remaining 0)
        t)))

(defgeneric active-p (lazy-stream)
  (:documentation "Returns t if the stream will return a value for next/peek. Otherwise false.
For non-streams always returns true. For 'nil' always returns false"))

(defmethod active-p ((stream lazy-stream))
  (remaining-p stream))

(defmethod active-p (obj)
  t)

(defmethod active-p ((obj null))
  nil)

(defgeneric reset (stream)
  (:documentation "Resets the stream to the init state, so it can be looped."))

(defmethod reset (obj))

(defmethod reset ((stream lazy-stream))
  (setf (lazy-stream-remaining stream)
         (lazy-stream-length stream)))

;; convenience methods for stream definitions
;; Convenience method for advancing substreams from gen-streams, which are a bit tricky
(declaim (inline next-substream))
(defun next-substream (substream)
  (next substream)
  (peek substream))

(declaim (inline create-substreams)
         (ftype (function (simple-vector) simple-vector) create-substreams))
(defun create-substreams (patterns)
  (map 'vector (lambda (pat) (make-stream pat))
       patterns))

(defmacro all-active (&rest streams)
  (cons 'and (mapcar (lambda (x) (list 'active-p x)) streams)))

(defun ensure-aref (vec i)
  (if (vectorp vec)
      (aref vec (mod i (length vec)))
      vec))

(defun ensure-length (vec)
  (if (vectorp vec)
      (length vec)
      1))

(defun %expand-arrays (body &key expand)
  (let ((vec-body body)
        (vec-s (gensym))
        (len-s (gensym)))
    (loop for x in expand
                        do (setf vec-body
                                 (subst
                                  `(ensure-aref ,x i)
                                  x vec-body )))
  `(if ,(cons 'or (loop for x in expand
                  collect (list 'vectorp x)))
       (let* ((,len-s ,(cons 'max
                             (loop for x in expand
                                   collect (list 'ensure-length x)
                                   )))
              (,vec-s (make-array ,len-s)))
         (loop for i below ,len-s
               do (setf (aref ,vec-s i)
                         ,vec-body))
         ,vec-s)
       ,body)))

(defmacro expand-arrays (body &key expand)
  (%expand-arrays body :expand expand))

;; generator base class
(defstruct (generator-pattern (:include pattern)
                              (:conc-name gen-pat-)))

(defstruct (generator-stream (:include lazy-stream)
                             (:conc-name gen-str-))
  value)

(defun make-gen-pattern (allocator length &rest arguments)
  (apply allocator :length length arguments))

(defun make-gen-stream (allocator pattern init-value &rest arguments)
  (let ((length (gen-pat-length pattern)))
    (apply allocator :length length
                     :remaining length
                     :value init-value
                     arguments)))


(defgeneric next-value-in-stream (stream)
  (:documentation "This is the method to override in order to
advance a generator stream."))

(defmethod next ((stream generator-stream))
  (if (active-p stream)
      (let ((value (gen-str-value stream)))
        (setf (gen-str-value stream) (next-value-in-stream stream))
        (dec-remaining stream)
        value)
      ;; next shouldn't be called by internal functions, as they should be checking
      ;; external users should use one of the utility functions
      (error 'end-of-stream)))

(defmethod peek ((stream generator-stream))
  (gen-str-value stream))

;; phrase base class
;; phrase is used for any pattern comprised up of a sequence of things

;; useful when initiating a pattern phrase, as it converts a passed in list to arrays
(defun convert-to-array (obj)
  (if (listp obj)
      (coerce
        (loop for x in obj
              collect (convert-to-array x))
        'simple-vector)
      obj))


(defstruct (pattern-phrase (:include pattern))
  (sequence #() :type simple-vector :read-only t)
  lace)

(defstruct (lazy-phrase (:include lazy-stream))
  (sequence #() :type simple-vector)
  (idx 0 :type fixnum)
  lace)

(defun make-phrase-pattern (allocator sequence cycles length lace &rest arguments)
  (let ((length (cond (length length)
                      (cycles (* cycles (length sequence)))
                      (t nil))))
    (apply allocator
           :sequence (convert-to-array sequence)
           :length length
           :lace lace
           arguments)))

(defun make-phrase-stream (allocator pattern init-index &rest arguments)
  (let ((length (pattern-phrase-length pattern)))
    (apply allocator :length length
                     :sequence (pattern-phrase-sequence pattern)
                     :remaining length
                     :lace (pattern-phrase-lace pattern)
                     :idx init-index
                     arguments)))

(defun phrase-aref (phrase idx)
  (let ((seq (lazy-phrase-sequence phrase)))
    (aref seq
          (mod idx (length seq)))))

(defun phrase-length (lazy-phrase)
  (length (lazy-phrase-sequence lazy-phrase)))

(defmethod reset ((phrase lazy-phrase))
  (call-next-method)
  (loop for x across (lazy-phrase-sequence phrase)
        do (reset x)))

(defgeneric next-phrase-idx (lazy-phrase))

(defmethod peek ((phrase lazy-phrase))
  (peek (phrase-aref phrase (lazy-phrase-idx phrase))))

(defmethod next ((stream lazy-phrase))
  (if (active-p stream)
      (let* ((idx (lazy-phrase-idx stream))
             (sub-stream (phrase-aref stream idx))
             (val  (next sub-stream)))
        (dec-remaining stream)
        (if (lazy-phrase-lace stream)
            (progn
              (when (and (lazy-stream-p sub-stream)
                         (not (active-p sub-stream)))
                (reset sub-stream))
              (setf (lazy-phrase-idx stream)
                    (mod (next-phrase-idx stream) (phrase-length stream))))
            (progn
              (unless (and (lazy-stream-p sub-stream)
                           (active-p sub-stream))
                (reset sub-stream)
                (setf (lazy-phrase-idx stream)
                      (mod (next-phrase-idx stream) (phrase-length stream))))))
        val)
      (error 'end-of-stream)))

(defmethod reset ((seq lazy-phrase))
  (call-next-method)
  (loop for x across (lazy-phrase-sequence seq)
        do (reset x)))
 
;; Array Pattern
(defstruct (pattern-array (:include pattern)
                          (:conc-name pat-arr-))
  (array #() :type simple-vector :read-only t))

(defstruct (lazy-array (:include lazy-stream)
                       (:conc-name lazy-arr-))
  (array #() :type simple-vector))

(defun make-stream ((pat pattern-array))
  (let* ((src-array (pat-arr-array pat))
         (dest-arr (make-array (length src-array))))
    (loop for x across src-array
          for i from 0
          do (setf (aref dest-arr i) x))
    
  )
