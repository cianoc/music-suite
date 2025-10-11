(defpackage music-suite/scales
  (:use #:cl #:music-suite/sequences #:music-suite/math)
  (:local-nicknames (:t :transducers) (:alex :alexandria) (:serap :serapeum))
  (:import-from :mgl-pax #:defsection )
  (:import-from :trivia #:match)
  (:export #:degree-to-key))
(in-package #:music-suite/scales)

(defstruct (scale (:constructor %make-scale))
  degrees
  tuning
  name
  base-freq)

(defun scale-length (scale)
  (length (scale-degrees scale)))

(defun make-scale (&key degrees tuning base-freq (name "Unknown Scale"))
  (%make-scale :degrees (coerce degrees 'vector)
               :tuning tuning
               :name name
               :base-freq base-freq))

(defun accidentals-from-degree (degree)
  (let ((scale-degree (round degree)))
    (round (* (- degree scale-degree) 10))))

(defun octaves-from-degree (degree scale-length)
  (floor (1- (round degree)) scale-length))
                    
(defun degree-freq (&key scale degree octave accidental down)
  (declare (ignore down))
  (let ((accidental (+ accidental
                        (accidentals-from-degree degree)))
        (octave (+ (octaves-from-degree degree
                                         (scale-length scale))
                    octave))
        (index (1- (round degree)))
        (base-freq (scale-base-freq scale)))
    (freq-from-tuning (scale-tuning scale)
                      base-freq
                      (+ (aref (scale-degrees scale) index) accidental)
                      octave)))



(defun chromatic-scale (&optional (tuning (et-tuning)))
  (let ((len (tuning-length tuning)))
    (make-scale :degrees (loop for x from 0 below len
                               collect x)
                :pitches-per-octave len
                :tuning tuning
                :name (format nil "Chromatic ~d (~d)"
                              len (tuning-name tuning)))))

(defun midi-ratio (midi)
  (expt 2.0 (/ midi 12.0)))

(defun ratio-midi (ratio)
  (* 12.0 (log ratio 2)))

(defun freq-cents (low-freq high-freq)
  (* 1200 (log (/ high-freq low-freq) 2)))

(defun cents (cents)
  (* cents (expt 2 (/ 1 1200))))


(defun degree-freq (scale degree &key octave accidental)
  (let* ((tuning (scale-tuning scale))
         (scale-degree (aref scale (round degree)))
         (accidental (+ accidental (* (- degree scale-degree) 10)))
         (octave-ratio (tuning-octave-ratio tuning)))
  )

	degreeToRatio { |degree, octave = 0|
		octave = octave + (degree div: degrees.size);
		^this.ratios.wrapAt(degree) * (this.octaveRatio ** octave);
	}

	degreeToFreq { |degree, rootFreq, octave|
		^this.degreeToRatio(degree, octave) * rootFreq;
	}
  
(defgeneric degree-to-key (scale degree &optional accidental))

(defmethod degree-to-key ((scale scale) degree &optional (accidental 0))
  (let* ((scale-degree (aref scale (round degree)))
         (accidental (+ accidental (* (- degree scale-degree) 10))))
    
    (perform-degree-to-key scale steps-per-octave)
  )
