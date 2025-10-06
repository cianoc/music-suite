(in-package :music-suite/patterns)

;; pwhite
(defstruct (pattern-white (:include generator-pattern)
                         (:conc-name pat-white-))
  low
  high
  (state *random-state* :type random-state :read-only t))

(defstruct (lazy-white (:include generator-stream))
  low
  high
  (state *random-state* :type random-state :read-only t))

(defmethod make-stream ((pat pattern-white))
  (let* ((state (pat-white-state pat))
         (low (make-stream (pat-white-low pat)))
         (high (make-stream (pat-white-high pat)))
         (low-val (peek low))
         (high-val (peek high))
         (val (expand-arrays
               (lin-rand low-val high-val state)
               :expand (low-val high-val))))
    (make-gen-stream #'make-lazy-white pat val 
                     :low low
                     :high high
                     :state state)))

(defmethod active-p ((stream lazy-white))
  (when (call-next-method)
    (all-active (lazy-white-low stream)
                (lazy-white-high stream))))

(defmethod next-value-in-stream ((stream lazy-white))
  (let ((state (lazy-white-state stream))
        (low (next-substream (lazy-white-low stream)))
        (high (next-substream (lazy-white-high stream))))
    (expand-arrays 
     (lin-rand low high state)
     :expand (low high))))
        
(defun pwhite (&key (low 0.0) (high 1.0) 
                       (state *random-state*) length)
  (make-gen-pattern #'make-pattern-white length
                    :low low :high high :state state))


; exprand
(defstruct (pattern-exprand (:include generator-pattern)
                            (:conc-name pat-exprand-))
  low
  high
  (state *random-state* :type random-state :read-only t))

(defstruct (lazy-exprand (:include generator-stream))
  low
  high
  (state *random-state* :type random-state :read-only t))

(defmethod make-stream ((pat pattern-exprand))
  (let* ((state (pat-exprand-state pat))
         (low (make-stream (pat-exprand-low pat)))
         (high (make-stream (pat-exprand-high pat)))
         (low-val (peek low))
         (high-val (peek high))
         (val (expand-arrays
               (exp-rand low-val high-val state)
               :expand (low-val high-val))))
    (make-gen-stream #'make-lazy-exprand pat val 
                     :low low
                     :high high
                     :state state)))

(defmethod active-p ((stream lazy-exprand))
  (when (call-next-method)
    (all-active (lazy-exprand-low stream)
                (lazy-exprand-high stream))))

(defmethod next-value-in-stream ((stream lazy-exprand))
  (let ((state (lazy-exprand-state stream))
        (low (next-substream (lazy-exprand-low stream)))
        (high (next-substream (lazy-exprand-high stream))))
    (expand-arrays 
     (exp-rand low high state)
     :expand (low high))))
        
(defun pexprand (&key (low 0.1) (high 1.0) 
                       (state *random-state*) length)
  (make-gen-pattern #'make-pattern-exprand length
                    :low low :high high :state state))

; brown 
(defstruct (pattern-brown (:include generator-pattern)
                            (:conc-name pat-brown-))
  low
  high
  step
  (state *random-state* :type random-state :read-only t))

(defstruct (lazy-brown (:include generator-stream)
                         (:conc-name lazy-brown-))
  low
  high
  step
  (state *random-state* :type random-state :read-only t))

(defmethod make-stream ((pat pattern-brown))
  (let* ((state (pat-brown-state pat))
         (low (make-stream (pat-brown-low pat)))
         (high (make-stream (pat-brown-high pat)))
         (step (make-stream (pat-brown-step pat)))
         (low-val (peek low))
         (high-val (peek high))
         (val (expand-arrays
               (lin-rand low-val high-val state)
               :expand (low-val high-val))))
    (make-gen-stream #'make-lazy-brown pat val 
                     :low low
                     :high high
                     :step step
                     :state state)))
  
(defmethod active-p ((stream lazy-brown))
  (when (call-next-method)
    (all-active (lazy-brown-low stream)
                (lazy-brown-high stream)
                (lazy-brown-step stream))))

(defmethod next-value-in-stream ((stream lazy-brown))
  (let* ((state (lazy-brown-state))
         (low (next-value-in-stream (lazy-brown-low stream)))
         (high (next-value-in-stream (lazy-brown-high stream)))
         (step (next (lazy-brown-step stream)))
         (val (lazy-brown-value stream)))
    (expand-arrays 
     (wrap (+ val (bi-rand step state)) low high)
     :expand (low high step val))))

(defun pbrown (&key (low 0.0) (high 10.0) (step 1.0)
                       (state *random-state*) length)
  (make-gen-pattern #'make-pattern-brown length
                    :low low :high high :step step :state state))

; pgbrown
(defstruct (pattern-gbrown (:include generator-pattern)
                          (:conc-name pat-gbrown-))
  low
  high
  step
  (state *random-state* :type random-state :read-only t))

(defstruct (lazy-gbrown (:include generator-stream)
                         (:conc-name lazy-gbrown-))
  low
  high
  step
  (state *random-state* :type random-state :read-only t))

(defmethod make-stream ((pat pattern-gbrown))
  (let* ((state (pat-gbrown-state pat))
         (low (make-stream (pat-gbrown-low pat)))
         (high (make-stream (pat-gbrown-high pat)))
         (step (make-stream (pat-gbrown-step pat)))
         (low-val (peek low))
         (high-val (peek high))
         (val (expand-arrays
               (lin-rand low-val high-val state)
               :expand (low-val high-val))))
    (make-gen-stream #'make-lazy-gbrown pat val 
                     :low low
                     :high high
                     :step step
                     :state state)))
  
(defmethod active-p ((stream lazy-gbrown))
  (when (call-next-method)
    (all-active (lazy-gbrown-low stream)
                (lazy-gbrown-high stream)
                (lazy-gbrown-step stream))))

(defmethod next-value-in-stream ((stream lazy-gbrown))
  (let* ((state (lazy-gbrown-state))
         (low (next-value-in-stream (lazy-gbrown-low stream)))
         (high (next-value-in-stream (lazy-gbrown-high stream)))
         (step (next (lazy-gbrown-step stream)))
         (val (lazy-gbrown-value stream)))
    (expand-arrays 
     (wrap (* val (1+ (bi-rand step state))) low high)
     :expand (low high step val))))

(defun pgbrown (&key (low 0.0) (high 10.0) (step 1.0)
                       (state *random-state*) length)
  (make-gen-pattern #'make-pattern-gbrown length
                    :low low :high high :step step :state state))


    
;; pgbrown
;; pbeta
;; pcauchy
;; pgauss
;; phprand
;; pmeanrand
;; Ppoisson
;; Pprob

;; Pwrand(list, weights, repeats)
;; Choose randomly, according to weighted probabilities (same as list.wchoose(weights)).

;; Random numbers, exponential distribution. Like exprand(lo, hi) .
;; Pbrown(lo, hi, step, length)
;; Brownian motion, arithmetic scale (addition).
