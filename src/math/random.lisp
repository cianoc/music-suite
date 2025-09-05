(in-package #:music-suite/math)

(defun rrand (low high &optional (state (make-random-state t)))
  (let ((limit (- high low)))
    (+ low (random limit state))))