(in-package :music-suite/scales)

(defstruct (tuning (:constructor %make-tuning))
  ratios
  (octave-ratio 2.0 :type single-float :read-only t)
  name
  base-freq)

(defstruct (tuning-et (:include tuning)
                      (:constructor %make-tuning-et)))

(defun make-tuning (&key ratios semitones
                      (octave-ratio 2.0)
                      pitches-per-octave
                      tuning-note tuning-freq
                      (name "Unknown Tuning"))
  
  (when semitones
    (setf ratios
          (semitones-ratios semitones octave-ratio)))
  (let* ((tuning
          (if pitches-per-octave
              (let ((ratios (make-array pitches-per-octave))
                    (frac (/ 1 pitches-per-octave)))
                (loop for i from 0 below pitches-per-octave
                      do (setf (aref ratios i)
                               (expt octave-ratio (* i frac))))
                (%make-tuning-et :ratios ratios
                                 :octave-ratio octave-ratio
                                 :name name
                                 :base-freq 1))
              (%make-tuning :ratios (coerce ratios 'vector)
                            :octave-ratio octave-ratio
                            :name name
                            :base-freq 1)))
         (base-freq (/ tuning-freq
                       (tune-freq-note tuning tuning-note))))
    (setf (tuning-base-freq tuning) base-freq)
    tuning))
    

(defun semitones-ratios (semitones octave-ratio)
  (let* ((len (length semitones))
         (vec (make-array len)))
    (loop for x across semitones
          for i :from 0
          do (setf (aref vec i)
                   (expt octave-ratio x)))
    vec))

(defun ratios-semitones (ratios octave-ratio)
  (let* ((len (length ratios))
         (vec (make-array len)))
    (loop for x across ratios
          for i :from 0
          do (setf (aref vec i)
                   (log x octave-ratio)))
    vec))

(defgeneric modal-scale (tuning root-degree)
  (:documentation "returns a rotated tuning, using root as the
starting point"))

(defmethod modal-scale ((tuning tuning-et) root-degree)
  tuning)

(defmethod modal-scale ((tuning tuning) root-degree)
  (let* ((semitones (ratios-semitones (tuning-ratios tuning)
                                      (tuning-octave-ratio tuning)))
         (len (length semitones))
         (octave-ratio (tuning-octave-ratio tuning))
         (vec (make-array len))
         (base (aref semitones root-degree))
         (base2 (- (log octave-ratio 2) base)))
    (loop for i from root-degree below len
          for j from 0
          do (setf (aref vec j)
                   (expt octave-ratio
                         (- (aref semitones i) base))))
    (loop for i from 0 below root-degree
          for j from (- len root-degree)
          do (setf (aref vec j)
                   (expt octave-ratio
                         (+ base2 (aref semitones i)))))
    
    (%make-tuning :ratios vec
                  :octave-ratio octave-ratio
                  :name (format nil "~a (mode: ~d)"
                                (tuning-name tuning)
                                root-degree)
                  :base-freq (tuning-base-freq tuning))))

(defun tune-freq-note (tuning note)
  (let* ((octave-ratio (tuning-octave-ratio tuning))
         (len (tuning-length tuning))
         (ratios (tuning-ratios tuning))
         (octave (truncate note len))
         (note (rem note len)))
    (* (tuning-base-freq tuning)
       (expt octave-ratio octave)
       (aref ratios note))))
    

(defun tuning-length (tuning)
  (length (tuning-ratios tuning)))


