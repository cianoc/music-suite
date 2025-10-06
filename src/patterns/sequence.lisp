(in-package :music-suite/patterns)

 
;; pseq
(defstruct (pattern-sequence (:include pattern-phrase)
                             (:conc-name pat-seq-)))
                             
(defstruct (lazy-sequence (:include lazy-phrase)
                          (:conc-name lazy-seq-)))

(defmethod make-stream ((pat pattern-sequence))
  (make-phrase-stream #'make-lazy-sequence pat 0))
  
(defmethod reset ((seq lazy-sequence))
  (call-next-method)
  (setf (lazy-seq-idx seq) 0))

(defmethod next-phrase-idx ((seq lazy-sequence))
    (1+ (lazy-seq-idx seq)))

(defun pseq (seq &key cycles length lace)
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
  (make-phrase-pattern #'make-pattern-sequence
                    seq cycles length lace))
    
;; prand
;; 
(defstruct (pattern-rand (:include pattern-phrase)
                         (:conc-name pat-rand-))
  (state *random-state* :type random-state :read-only t))
                             
(defstruct (lazy-rand (:include lazy-phrase)
                      (:conc-name lazy-rand-))
  (state *random-state* :type random-state :read-only t))

(defmethod make-stream ((pat pattern-rand))
  (let* ((state (pat-rand-state pat))
         (stream (make-phrase-stream #'make-lazy-rand pat 0
                  :state state)))
    (setf (lazy-rand-idx stream)
          (random (phrase-length stream)
                (lazy-rand-state stream)))
    stream))

(defmethod reset ((stream lazy-rand))
  (call-next-method)
  (setf (lazy-rand-idx stream)
        (next-phrase-idx stream)))

(defun prand (seq &key length cycles lace (state *random-state*))
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

  (make-phrase-pattern #'make-pattern-rand
                       seq cycles length lace
                       :state state))

(defmethod next-phrase-idx ((stream lazy-rand))
  (random (phrase-length stream)
          (lazy-rand-state stream)))

;; pxrand
(defstruct (pattern-xrand (:include pattern-rand)
                          (:conc-name pat-xrand-)))

(defstruct (lazy-xrand (:include lazy-rand)
                      (:conc-name lazy-xrand-)))

(defun pxrand (seq &key length cycles lace (state *random-state*))
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
  (make-phrase-pattern #'make-pattern-xrand
                       seq cycles length lace
                       :state state))

(defmethod next-phrase-idx ((stream lazy-xrand))
  (1+ (random (1- (phrase-length stream))
              (lazy-rand-state stream))))

;; PShuffle
(defstruct (pattern-shuffle (:include pattern-phrase)
                            (:conc-name pat-shuf-))
  (state *random-state* :type random-state :read-only t)
  shuffle-every)

(defstruct (lazy-shuffle (:include lazy-phrase)
                         (:conc-name lazy-shuf-))
  (state *random-state* :type random-state :read-only t)
  shuffle-every
  shuffle-count)

(defmethod make-stream ((pat pattern-shuffle))
  (let* ((state (pat-shuf-state pat))
         (shuffle-every (make-stream (pat-shuf-shuffle-every pat)))
         (stream (make-phrase-stream #'make-lazy-shuffle pat 0
                  :state state
                  :shuffle-every shuffle-every
                  :shuffle-count (next shuffle-every))))
    (shuffle-in-place (lazy-shuf-sequence stream)
                      (lazy-shuf-state stream))
    stream))

(defmethod reset ((seq lazy-shuffle))
  (call-next-method)
  (setf (lazy-shuf-idx seq) 0)
  (reset (lazy-shuf-shuffle-every seq))
  (setf (lazy-shuf-shuffle-count seq) (next (lazy-shuf-shuffle-every seq))))

(defmethod next-phrase-idx ((seq lazy-shuffle))
  (when (lazy-shuf-shuffle-count seq)
    (if (= 0 (lazy-shuf-shuffle-count seq))
        (if (active-p (lazy-shuf-shuffle-count seq))
            (progn
              (shuffle-in-place (lazy-shuf-sequence seq)
                                (lazy-shuf-state seq))
              (setf (lazy-shuf-shuffle-count seq)
                    (next (lazy-shuf-shuffle-every seq))))
            (make-condition 'end-of-stream))
        (decf (lazy-shuf-shuffle-count seq))))
    (1+ (lazy-shuffle-idx seq)))

(defmethod active-p ((seq lazy-shuffle))
  (when (call-next-method)
    (if (and (lazy-shuf-shuffle-count seq)
             (= 0 (lazy-shuf-shuffle-count seq)))
        (active-p (lazy-shuf-shuffle-every seq))
        t)))

(defun pshuf (seq &key length cycles shuffle-every shuffle-cycle lace (state *random-state*))
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
  (let ((every (cond (shuffle-every shuffle-every)
                     (shuffle-cycle (* shuffle-cycle (length seq)))
                     (t nil))))
  (make-phrase-pattern #'make-pattern-rand
                       seq cycles length lace
                       :shuffle-every every
                       :state state)))

