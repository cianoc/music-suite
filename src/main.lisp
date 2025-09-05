(defpackage music-patterns
  (:use #:cl)
  (:local-nicknames (:t :transducers) (:alex :alexandria) (:serap :serapeum))
  (:import-from :mgl-pax #:defsection )
  (:import-from :trivia #:match))
(in-package #:music-patterns)

;; Used internally for typechecking.
(deftype maybe-fixnum () '(or null fixnum))

(defstruct (pattern)
  "The root class for all patterns. A pattern is a template for creating 
streams. You will initiate a pattern with it's parameters, and then you can create
multiple streams from that pattern, each of which are fully independent of each other")

(defgeneric create-stream (pattern)
  (:documentation "When passed a pattern, this will create a new stream.

This must be overriden by all subclasses of pattern"))

(defstruct lazy-stream
  "This is the root class for all lazy-streams. Lazy-streams are created from a pattern.

Lazy-streams are created one time and return values until they are used up.")

(defgeneric next (lazy-stream)
  (:documentation "This will advance the stream and return the next value in the stream. If
the stream is finished then this will return nil.

This must be overriden by all subclasses of pattern"))

(defgeneric peek (lazy-stream)
  (:documentation "This allows you to see the next value in the stream, without advancing
the stream. This is typically something that should only be used by other sequence objects"))


;;;;; pseq ;;;;;;;;;;

(defstruct (pattern-sequence (:include pattern)
                             (:conc-name pat-seq-))
  (sequence #() :type simple-vector)
  (cycles nil :type maybe-fixnum)
  (offset 0 :type fixnum))

(defstruct (lazy-sequence (:include lazy-stream)
                            (:conc-name lazy-seq-))
  (idx 0 :type fixnum)
  (sequence #() :type simple-vector)
  (remaining nil :type maybe-fixnum))

(defmethod create-stream ((pat pattern-sequence))
  (let* ((seq (pat-seq-sequence pat))
         (size (fset:size seq))
         (cycles (pat-seq-cycles pat)))
    (make-lazy-sequence
     :idx (mod (pat-seq-offset pat) size)
     :sequence seq
     :remaining (when cycles (* size cycles)))))

(defmethod peek ((seq lazy-sequence))
  (fset:lookup (lazy-seq-sequence seq) (lazy-seq-idx seq)))

(defmethod next ((seq lazy-sequence))
  (let ((remaining (lazy-seq-remaining seq)))
  (unless (and remaining (= 0 remaining))
    (let* ((ret-val (fset:lookup (lazy-seq-sequence seq) 
                                      (lazy-seq-idx seq)))
           (idx (lazy-seq-idx seq))
           (size (fset:size (lazy-seq-sequence seq)))
           (remaining (lazy-seq-remaining seq)))
      (setf (lazy-seq-idx seq) (mod (1+ idx) size))
      (when remaining
        (setf (lazy-seq-remaining seq) (1- remaining)))
      ret-val))))
      
;;;;; prand ;;;;;
(defstruct (pattern-rand (:include pattern)
                         (:conc-name pat-rand-))
  (sequence #() :type simple-vector)
  (cycles nil :type maybe-fixnum))

(defstruct (lazy-rand (:include lazy-stream)
                      (:conc-name lazy-rand-))
  state ;; stores the random state
  next
  (sequence #() :type simple-vector)
  (remaining nil :type maybe-fixnum))

(defmethod create-stream ((pat pattern-rand))
  (let* ((seq (pat-rand-sequence pat))
         (size (fset:size seq))
         (cycles (pat-rand-cycles pat))
         (state (make-random-state))
         (remaining (when cycles (* size cycles))))
    (make-lazy-rand
     :sequence seq
     :state state
     :next (unless (and cycles (<= cycles 0))
             (fset:lookup seq (random size state)))
     :remaining remaining)))

(defmethod peek ((stream lazy-rand))
  (lazy-rand-next stream))

(defmethod next ((stream lazy-rand))
  (let ((remaining (lazy-rand-remaining stream)))
    (unless (and remaining (equal 0 remaining))
      (let* ((size (fset:size (lazy-rand-sequence stream)))
             (next (fset:lookup (lazy-rand-sequence stream) 
                   (random size (lazy-rand-state stream))))
             (ret-val (lazy-rand-next stream)))
        (when remaining
          (setf (lazy-rand-remaining stream)
                (1- remaining)))
        (setf (lazy-rand-next stream) next)
        ret-val))))




(defun pseq (seq &key cycles (offset 0) selection)
  "Create a pseq pattern. A pseq pattern takes a sequence (vector, array, list)
and lets you define a sequence based upon it.

### Parameters

**seq**

A sequence of values (list, array, etc) that pseq will cycle over. Any sequence is fine so long as it
can be converted to fset:seq.

Subsequent changes to the sequence will have no affect on the pseq, as it creates an immutable local copy.

**cycles**

How many times the pattern should cycle through the sequence that is passed in.

A value of 1 means that the stream will return the values in the sequence and then stop. A value of 2
will return the values in the sequence twice. If you do not set this value then the sequence will loop
for ever.

**offset**
Where to start the pattern. A value of 0 (the default) means that the sequence will start from the first
value. A value of 1 means that the sequence will start from the second value.

Offset has no effect on how many values are returned by the stream, it simply changes the start and stop values.
So if you pass in an offset of 2 - the lazy-streams will start with the 2nd value, and end with the 1st value.

If the value passed in is larger than the size of the sequence, then the offset will simply be the remainder.
E.g. if the length of the sequence is 4, and you pass in an offset of 5 - then a value of 1 will be used.
"
  (match selection
    ('random (make-pattern-rand :sequence (coerce seq 'simple-vector)
                                :cycles cycles))
    (nil (make-pattern-sequence :sequence (coerce seq 'simple-vector)
                                :cycles cycles
                                :offset offset))))
;; blah blah blah.