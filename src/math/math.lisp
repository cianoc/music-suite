(defpackage music-suite/math
  (:use #:cl #:music-suite/utils)
  (:local-nicknames (:t :transducers) (:alex :alexandria) (:serap :serapeum))
  (:import-from :mgl-pax #:defsection )
  (:export #:2pi
           #:lin-lin
           #:lin-exp
           #:exp-lin
           #:exp-exp
           #:fold
           #:wrap
           #:rand
           #:lin-rand
           #:bi-rand
           #:exp-rand
           #:coin
           #:gauss
           ))
(in-package #:music-suite/math)

(defconstant 2pi (* 2 pi))