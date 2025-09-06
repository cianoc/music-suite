(defpackage music-suite/patterns
  (:use #:cl #:music-suite/sequences #:music-suite/math)
  (:local-nicknames (:t :transducers) (:alex :alexandria) (:serap :serapeum))
  (:import-from :mgl-pax #:defsection )
  (:import-from :trivia #:match))
(in-package #:music-suite/patterns)

;; Used internally for typechecking.
(deftype maybe-fixnum () '(or null fixnum))

(defstruct (pattern)
  "The root class for all patterns. A pattern is a template for creating 
streams. You will initiate a pattern with it's parameters, and then you can create
multiple streams from that pattern, each of which are fully independent of each other")

(defgeneric create-stream (pattern)
  (:documentation "When passed a pattern, this will create a new stream.

This must be overriden by all subclasses of pattern"))

(defmethod create-stream (obj)
  obj)

(defstruct lazy-stream
  "This is the root class for all lazy-streams. Lazy-streams are created from a pattern.

Lazy-streams are created one time and return values until they are used up.")

(defgeneric next (lazy-stream)
  (:documentation "This will advance the stream and return the next value in the stream. If
the stream is finished then this will return nil.

This must be overriden by all subclasses of pattern"))

(defmethod next (obj)
  obj)

(defgeneric peek (lazy-stream)
  (:documentation "This allows you to see the next value in the stream, without advancing
the stream. This is typically something that should only be used by other sequence objects"))

(defmethod peek (obj)
  obj)



      





