(in-package #:music-suite/sequences)

(defgeneric shuffle (seq)
  (:documentation "Shuffle a sequence. It will return a sequence with the same type as that passed in."))

(defun shuffle-vec (vec)
  (let ((size (length vec)))
    (loop for i :from 0 :to (- size 2)
          for j = (math::rrand i size)
          do (rotatef (aref vec i) (aref vec j))))
  vec)

(defmethod shuffle ((val null))
  nil)

(defmethod shuffle ((lst cons))
  (let ((vec (coerce lst 'simple-vector)))
    (coerce (shuffle-vec vec) 'list)))

(defmethod shuffle ((vec vector))
  (let ((vec2 (copy-array vec)))
    (shuffle-vec vec2)))