(defpackage music-suite/sequences
  (:use #:cl #:music-suite/utils)
  (:local-nicknames (:t :transducers) 
                    (:alex :alexandria) 
                    (:serap :serapeum)
                    (:math :music-suite/math))
  (:import-from :mgl-pax #:defsection )
  (:export :shuffle))
(in-package #:music-suite/sequences)