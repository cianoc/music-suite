(in-package :music-suite/patterns)

;; pseries
(defstruct (pattern-series (:include pattern)
                           (:conc-name pat-series-))
  start
  step
  (length nil :type maybe-fixnum))

(defstruct (lazy-series (:include pattern))
  val
  step
  (remaining nil :type maybe-fixnum))

(defmethod peek ((stream lazy-series))
  (lazy-series-val stream))

(defmethod next ((stream lazy-series))
  (let ((remaining (lazy-series-remaining stream))
        (val (lazy-series-val stream))
        (step (next (lazy-series-step stream))))
    (when (and val
               (not (and remaining (<= remaining 0))))
      (if step
          (incf (lazy-series-val stream) step)
          (setf (lazy-series-val stream) nil))
      (when remaining
        (decf (lazy-series-remaining stream)))
      val)))

(defmethod create-stream ((pat pattern-series))
  (make-lazy-series
   :val (pat-series-start pat)
   :step (create-stream 
          (pat-series-step pat))
   :remaining (pat-series-length pat)))

(defun pat-series (&key (start 0) (step 1) length)
  (make-pattern-series :start start :step step :length length))

;; Pgeom(start, grow, length)
(defstruct (pattern-geom (:include pattern)
                           (:conc-name pat-geom-))
  start
  mul
  (length nil :type maybe-fixnum))

(defstruct (lazy-geom (:include pattern))
  val
  mul
  (remaining nil :type maybe-fixnum))

(defmethod peek ((stream lazy-geom))
  (lazy-geom-val stream))

(defmethod next ((stream lazy-geom))
  (let ((remaining (lazy-geom-remaining stream))
        (val (lazy-geom-val stream))
        (mul (next (lazy-geom-mul stream))))
    (when (and val
               (not (and remaining (<= remaining 0))))
      (if mul
          (setf (lazy-geom-val stream) 
                (* val mul))
          (setf (lazy-geom-val stream) nil))
      (when remaining
        (decf (lazy-geom-remaining stream)))
      val)))

(defmethod create-stream ((pat pattern-geom))
  (make-lazy-geom
   :val (pat-geom-start pat)
   :mul (create-stream 
         (pat-geom-mul pat))
   :remaining (pat-geom-length pat)))

(defun pat-geom (&key (start 1) (mul 2) length)
  (make-pattern-geom :start start :mul mul :length length))

;; Geometric series (multiplication).