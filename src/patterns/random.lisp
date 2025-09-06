(in-package :music-suite/patterns)

;; pwhite
(defstruct (pattern-white (:include pattern)
                         (:conc-name pat-white-))
  low
  high
  (length nil :type maybe-fixnum))

(defstruct (lazy-white (:include lazy-stream))
  low
  high
  (remaining nil :type maybe-fixnum)  
  val
  state)

(defmethod peek ((stream lazy-white))
  (lazy-white-val stream))

(defmethod next ((stream lazy-white))
  (let ((val (lazy-white-val stream))
        (remaining (lazy-white-remaining stream))
        (high (next (lazy-white-high stream)))
        (low (next (lazy-white-low stream)))
        (state (lazy-white-state stream)))
    (when (and val high low)
      (setf (lazy-white-val stream)
            (if (and remaining (>= remaining 0))
                nil
                (lin-rand low high state))))
    val))

(defmethod create-stream ((pat pattern-white))
  (let* ((low-stream (create-stream (pat-white-low pat)))
         (high-stream (create-stream (pat-white-high pat)))
         (lo (next low-stream))
         (hi (next high-stream))
         (remaining (pat-white-length pat)))
    (if (and hi lo 
             (not (and remaining (<= remaining 0))))
        (let* ((state (make-random-state))
               (nxt (lin-rand lo hi state)))
          (make-lazy-white :val nxt
                           :remaining remaining
                           :low low-stream
                           :high high-stream
                           :state state))
        (make-lazy-white))))
         

(defun pat-white (&key (low 0.0) (high 0.0) 
                       length)
  (make-pattern-white :low low :high high :length length))


    
;; prand

;; Pwrand(list, weights, repeats)
;; Choose randomly, according to weighted probabilities (same as list.wchoose(weights)).

;; Pwhite(lo, hi, length)
;; Random numbers, equal distribution ("white noise"). Like rrand(lo, hi) .
;; Pexprand(lo, hi, length)
;; Random numbers, exponential distribution. Like exprand(lo, hi) .
;; Pbrown(lo, hi, step, length)
;; Brownian motion, arithmetic scale (addition).