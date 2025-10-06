(in-package :music-suite/patterns)

;; pseries
(defstruct (pattern-series (:include generator-pattern)
                           (:conc-name pat-series-))
  start
  step)

(defstruct (lazy-series (:include generator-stream))
  step
  init-value)

(defmethod make-stream ((pat pattern-series))
  (let ((init-value (pat-series-start pat)))
    (make-gen-stream #'make-lazy-series pat init-value
                     :init-value init-value
                     :step (make-stream (pat-series-step pat)))))

(defmethod active-p ((stream lazy-series))
  (when (call-next-method)
    (active-p (lazy-series-step stream))))

(defmethod next-value-in-stream ((stream lazy-series))
  (let ((step (next (lazy-series-step stream)))
        (value (lazy-series-value stream)))
    (expand-arrays
     (+ step value)
     :expand (step value))))

;; (defmethod next-value-in-stream ((stream lazy-series))
;;   (let ((step (next (lazy-series-step stream)))
;;         (value (lazy-series-value stream)))
;;     (if (or (vectorp value)(vectorp step))
;;         (let* ((length (max (ensure-length step)
;;                             (ensure-length value)))
;;                (vec (make-array length)))
;;           (loop for i below length
;;                 do (setf (aref vec i)
;;                          (+ (ensure-aref value i)
;;                             (ensure-aref step i))))
;;           vec)
;;         (+ (lazy-series-value stream)
;;            (next (lazy-series-step stream))))))

;;           )
;;     )
;;   (+ (lazy-series-value stream)
;;      (next (lazy-series-step stream))))

(defun pat-series (&key (start 0) (step 1) length)
  (make-gen-pattern #'make-pattern-series length
                    :start start
                    :step step))

(defmethod reset ((stream lazy-series))
  (setf (lazy-series-value stream)
        (lazy-series-init-value stream))
  (reset (lazy-series-step stream))
  (call-next-method))

;; Pgeom(start, grow, length)
(defstruct (pattern-geom (:include generator-pattern)
                         (:conc-name pat-geom-))
  (start 1 :type number :read-only t)
  mul)

(defstruct (lazy-geom (:include generator-stream))
  mul
  init-value)

(defmethod make-stream ((pat pattern-geom))
  (let ((init-value (pat-geom-start pat)))
    (make-gen-stream #'make-lazy-geom pat init-value
                     :init-value init-value
                     :mul (make-stream (pat-geom-mul pat)))))

(defmethod active-p ((stream lazy-geom))
  (when (call-next-method)
    (active-p (lazy-geom-mul stream))))

(defmethod next-value-in-stream ((stream lazy-geom))
  (* (lazy-geom-value stream)
     (next (lazy-geom-mul stream))))

(defun pat-geom (&key (start 1) (mul 2) length)
  (make-gen-pattern #'make-pattern-geom length :start start :mul mul))

;; Geometric series (multiplication).
