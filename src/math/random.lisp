(in-package #:music-suite/math)

(defun rand (&optional (val 1.0) (state (make-random-state t)))
  "Essentially the same as random. Just to keep naming standard. 
Will create a new state if one is not passed in"
  (random val state))

(defun lin-rand (low high &optional (state (make-random-state t)))
  "A random number between low and high"
  (lin-lin (random 1.0 state)
          :in1 0 :in2 1
           :out1 low :out2 high))

(defun bi-rand (val &optional (state (make-random-state t)))
  "like random, except the range is -val ->val"
  (lin-lin (random 1.0 state)
           :in1 0 :in2 1
           :out1 (- val) :out2 val))

(defun exp-rand (low high &optional (state (make-random-state t)))
  "Exponentially distributed random number between high and low"
  (lin-exp (random 1.0 state)
           :in1 0 :in2 1
           :out1 low :out2 high))

(defun coin (&optional (probability 0.5) (state (make-random-state t)))
  "randomly returns T, or F.
- probability - 0 -> 1.0. The probability that T will be returned. For an unbaised coin it should be 0.5
- state - a random state generator"
  (let ((val (random 1.0 state)))
    (if (> val probability) T nil)))

(defun gauss (&key (mean 0.0) (sd 1.0) (state (make-random-state t)))
  "a gaussian distributed random number.
- sd (standard deviation)"
  (+ mean
     (* sd
        (sqrt (* -2 (log (rand 1.0 state))))
        (sin (rand 2pi state)))))