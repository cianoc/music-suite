(in-package #:music-suite/sequences)

(defgeneric shuffle (seq)
  (:documentation "Shuffle a sequence. It will return a sequence with the same type as that passed in."))

(defun shuffle-in-place (vec &optional (state *random-state*))
  (let ((size (length vec)))
    (loop for i :from 0 :to (- size 2)
          for j = (+ i (random (- size i) state))
          do (rotatef (aref vec i) (aref vec j))))
  vec)

(defmethod shuffle ((val null))
  nil)

(defmethod shuffle ((lst cons))
  (let ((vec (coerce lst 'simple-vector)))
    (coerce (shuffle-in-place vec) 'list)))

(defmethod shuffle ((vec vector))
  (let ((vec2 (copy-array vec)))
    (shuffle-in-place vec2)))
