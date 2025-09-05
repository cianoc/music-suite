(in-package #:music-suite/patterns)

;; First let's create the abstract classes underpinning this.
(defstruct (pattern-vector (:include pattern)
                           (:conc-name pat-vec-))
  (sequence #() :type simple-vector)
  (cycles nil :type maybe-fixnum))

(defstruct (lazy-vector (:include lazy-stream)
                          (:conc-name lazy-vec-))
  (idx 0 :type fixnum)
  (sequence #() :type simple-vector)
  (remaining nil :type maybe-fixnum))

(defmethod peek ((stream lazy-vector))
  (let* ((remaining (lazy-vec-remaining stream))
         (seq (lazy-vec-sequence stream))
         (size (length seq))
         (idx (lazy-vec-idx stream)))
    (unless (and remaining (<= remaining 0))
      (aref seq (mod idx size)))))

(defgeneric lazy-next-idx (stream))

(defmethod next ((seq lazy-vector))
  (let ((val (peek seq)))
    (when val
      (when (lazy-vec-remaining seq)
        (decf (lazy-vec-remaining seq)))
      (setf (lazy-vec-idx seq) 
            (lazy-next-idx seq)))
    val))

;;;;; pseq ;;;;;;;;;;

(defstruct (pattern-sequence (:include pattern-vector)
                             (:conc-name pat-seq-))
  (offset 0 :type fixnum))

(defstruct (lazy-sequence (:include lazy-vector)
                          (:conc-name lazy-seq-)))    

(defmethod create-stream ((pat pattern-sequence))
  (let* ((seq (pat-seq-sequence pat))
         (size (fset:size seq))
         (cycles (pat-seq-cycles pat)))
    (make-lazy-sequence
     :idx (mod (pat-seq-offset pat) size)
     :sequence seq
     :remaining (when cycles (* size cycles)))))

(defmethod lazy-next-idx ((stream lazy-sequence))
  (let ((idx (lazy-vec-idx stream))
        (size (length (lazy-vec-sequence stream))))
    (mod (incf idx) size)))

(defun pat-seq (seq &key cycles (offset 0))
"Create a pseq pattern. A pseq pattern takes a sequence (vector, array, list)
and lets you define a sequence based upon it.

### Parameters

**seq**

A sequence of values (list, array, etc) that pseq will cycle over. Any sequence is fine so long as it
can be converted to an array.

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
(make-pattern-sequence :sequence (coerce seq 'simple-vector)
                       :cycles cycles
                       :offset offset))


;;;;; prand ;;;;;
(defstruct (pattern-rand (:include pattern-vector)
                         (:conc-name pat-rand-)))

(defstruct (lazy-rand (:include lazy-vector)
                      (:conc-name lazy-rand-))
  state)

(defmethod create-stream ((pat pattern-rand))
  (let* ((seq (pat-rand-sequence pat))
         (size (length seq))
         (cycles (pat-rand-cycles pat))
         (state (make-random-state))
         (remaining (when cycles (* size cycles))))
    (make-lazy-rand
     :sequence seq
     :idx (random size state)
     :state state
     :remaining remaining)))

(defmethod lazy-next-idx ((stream lazy-rand))
  (random (length (lazy-rand-sequence stream)) 
          (lazy-rand-state stream)))

(defun pat-rand-seq (seq &key cycles)
  "Create a random pseq pattern. A pseq pattern takes a sequence (vector, array, list) and then randomly plays an element from that sequence each time.

### Parameters

**seq**

A sequence of values (list, array, etc) that this will use. Any sequence is fine so long as it
can be converted to an array.

Subsequent changes to the sequence will have no affect on the pseq, as it creates an immutable local copy.

**cycles**

This affects how many times the stream will play.

A value of 1 means that the stream will play for the length of the sequence passed in. 
A value of 2 will play for 2x the length of the sequence.
If you do not set this value then the sequence will loop for ever."

  (make-pattern-rand :sequence (coerce seq 'simple-vector)
                     :cycles cycles))

;; pxrand
(defstruct (pattern-xrand (:include pattern-vector)
                          (:conc-name pat-xrand-)))

(defstruct (lazy-xrand (:include lazy-vector)
                       (:conc-name lazy-xrand-))
  state)

(defmethod create-stream ((pat pattern-xrand))
  (let* ((seq (pat-xrand-sequence pat))
         (size (length seq))
         (cycles (pat-xrand-cycles pat))
         (state (make-random-state))
         (remaining (when cycles (* size cycles))))
    (make-lazy-xrand
     :sequence seq
     :idx (random size state)
     :state state
     :remaining remaining)))

(defmethod lazy-next-idx ((stream lazy-xrand))
  (let* ((size (length (lazy-xrand-sequence stream)))
         (rand (1+ (random (1- size)
                           (lazy-xrand-state stream))))
         (idx (lazy-xrand-idx stream)))
    (mod (+ idx rand) size)))

(defun pat-xrand-seq (seq &key cycles)
  "Create a random pseq pattern. A pseq pattern takes a sequence (vector, array, list) and then randomly plays an element from that sequence each time.

Unlike pat-rand-seq - this will never play the same value from the array
twice in a row.

### Parameters

**seq**

A sequence of values (list, array, etc) that this will use. Any sequence is fine so long as it
can be converted to an array.

Subsequent changes to the sequence will have no affect on the pseq, as it creates an immutable local copy.

**cycles**

This affects how many times the stream will play.

A value of 1 means that the stream will play for the length of the sequence passed in. 
A value of 2 will play for 2x the length of the sequence.
If you do not set this value then the sequence will loop for ever."

  (make-pattern-xrand :sequence (coerce seq 'simple-vector)
                      :cycles cycles))

;;;;; pshuffle;;;;;
(defstruct (pattern-shuffle (:include pattern-vector)
                         (:conc-name pat-shuf-)))

(defstruct (lazy-shuffle (:include lazy-vector)
                      (:conc-name lazy-shuf-)))

(defmethod create-stream ((pat pattern-shuffle))
  (let* ((seq (pat-shuf-sequence pat))
         (size (length seq))
         (cycles (pat-shuf-cycles pat))
         (remaining (when cycles (* size cycles))))
    (make-lazy-shuffle
     :sequence (shuffle seq)
     :idx 0
     :remaining remaining)))

(defmethod lazy-next-idx ((stream lazy-rand))
  (random (length (lazy-rand-sequence stream)) 
          (lazy-rand-state stream)))

(defun pat-shuf-seq (seq &key cycles)
  "Create a pseq pattern. A pat-shuf-seq pattern takes a sequence (vector, array, list)
shuffles it and then constructs a stream from it.

Each time a stream is created it will be randomly shuffled, but a stream
will cycle over the same shuffled sequence.

### Parameters

**seq**

A sequence of values (list, array, etc) that pseq will cycle over. Any sequence is fine so long as it
can be converted to an array.

Subsequent changes to the sequence will have no affect on the pseq, as it creates an immutable local copy.

**cycles**

This affects how many times the stream will play.

A value of 1 means that the stream will play for the length of the sequence passed in. 
A value of 2 will play for 2x the length of the sequence.
If you do not set this value then the sequence will loop for ever.
"
  (make-pattern-shuffle :sequence (coerce seq 'simple-vector)
                        :cycles cycles))
