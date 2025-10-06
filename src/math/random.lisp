(in-package #:music-suite/math)

;; lets be able to use rand rather than random, just to make things easier to remember.
(serapeum:defalias rand #'random)

(declaim (ftype (function (number number &optional random-state) number) lin-rand)
         (inline lin-rand))
(defun lin-rand (low high &optional (state *random-state*))
  (declare (optimize (speed 3) (safety 1)))
  (let ((val (random (- high low) state)))
    (declare (type number val))
    (+ low val)))

(declaim (ftype (function (number &optional random-state) number) bi-rand)
         (inline bi-rand))
(defun bi-rand (high &optional (state *random-state*))
  (declare (optimize (speed 3) (safety 1)))
  (let ((val (random (* 2 high) state)))
    (declare (type number val))
    (- val high)))

(declaim (ftype (function (number number &optional random-state) number) exp-rand)
         (inline exp-rand))
(defun exp-rand (low high &optional (state *random-state*))
  "Exponentially distributed random number between high and low"
  (declare (optimize (speed 3) (safety 1)))
  (lin-exp (random 1.0 state)
           :in1 0.0 :in2 1.0
           :out1 low :out2 high))

(declaim (ftype (function (&optional float random-state)) coin)
         (inline coin))
(defun coin (&optional (probability 0.5) (state *random-state*))
  "randomly returns T, or F.
- probability - 0 -> 1.0. The probability that T will be returned. For an unbaised coin it should be 0.5
- state - a random state generator"
  (declare (optimize (speed 3) (safety 1)))
  (let ((val (random 1.0 state)))
    (declare (single-float val))
    (if (> val probability) T nil)))

(declaim (ftype (function (&key (:mean float)
                                (:sd float)
                                (:state random-state)) float) gauss)
         (inline gauss))
(defun gauss (&key (mean 0.0) (sd 1.0) (state *random-state*))
  "a gaussian distributed random number.
- sd (standard deviation)"
  (declare (optimize (speed 3) (safety 1)))
  (+ mean
     (* sd
        (sqrt (* -2 (log (rand 1.0 state))))
        (sin (rand 2pi state)))))
