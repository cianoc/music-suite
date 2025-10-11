(defpackage music-suite/scales
  (:use #:cl #:music-suite/sequences #:music-suite/math)
  (:local-nicknames (:t :transducers) (:alex :alexandria) (:serap :serapeum))
  (:import-from :mgl-pax #:defsection )
  (:import-from :trivia #:match)
  (:export #:degree-to-key))
(in-package #:music-suite/scales)

(defstruct (scale (:constructor %make-scale))
  degrees
  pitches-per-octave
  tuning
  name)

(defun make-scale (&key degrees pitches-per-octave tuning name)
  (setf degrees (coerce degrees 'vector))
  (unless pitches-per-octave
    (let ((last-degree (aref degrees (1- (length degrees))))
          (et-types #(12 19 24 53 128)))
      (setf pitches-per-octave
            (loop for x across et-types
                  when (< last-degree x)
                    return x
                  finally
                     (return x)))))
  (if tuning
      (when (/= pitches-per-octave (tuning-length tuning))
        (error (format nil
                       "Scale steps per octave:
~d does not match tuning size: ~d"
               pitches-per-octave
               (tuning-length tuning))))
      (setf tuning (default-tuning pitches-per-octave)))
  (%make-scale :degrees degrees
               :pitches-per-octave pitches-per-octave
               :tuning tuning
               :name name))

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
