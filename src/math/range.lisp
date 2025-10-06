(in-package :music-suite/math)

(defun lin-lin (in &key (in1 0.0) (in2 1.0) out1 out2 (clip :min-max))
  (match clip
    (:min-max
     (when (< in in1) (return-from lin-lin out1))
     (when (> in in2) (return-from lin-lin out2)))
    (:min
     (when (< in in1) (return-from lin-lin out1)))
    (:max
     (when (> in in2) (return-from lin-lin out2))))
  (+ (*
      (/ (- in in1) (- in2 in1)) 
      (- out2 out1))
     out1))

(defun lin-exp (in &key (in1 0) (in2 1) out1 out2 (clip :min-max))
  (match clip
    (:min-max
     (when (< in in1) (return-from lin-exp out1))
     (when (> in in2) (return-from lin-exp out2)))
    (:min
     (when (< in in1) (return-from lin-exp out1)))
    (:max
     (when (> in in2) (return-from lin-exp out2))))
  (* out1 
     (expt
      (/ out2 out1)
      (/ (- in in1)
         (- in2 in1)))))

(defun exp-lin (in &key (in1 0) (in2 1) out1 out2 (clip :min-max))
  (match clip
    (:min-max
     (when (< in in1) (return-from exp-lin out1))
     (when (> in in2) (return-from exp-lin out2)))
    (:min
     (when (< in in1) (return-from exp-lin out1)))
    (:max
     (when (> in in2) (return-from exp-lin out2))))
  (+ out1
     (* (- out2 out1)
        (/ (log (/ in in1))
           (log (/ in2 in1))))))

(defun exp-exp (in &key (in1 0.1) (in2 10) out1 out2 (clip :min-max))
  (match clip
    (:min-max
     (when (< in in1) (return-from exp-exp out1))
     (when (> in in2) (return-from exp-exp out2)))
    (:min
     (when (< in in1) (return-from exp-exp out1)))
    (:max
     (when (> in in2) (return-from exp-exp out2))))
  (* out1
     (expt (/ out2 out1)
           (/ (log (/ in in1))
              (log (/ in2 in1))))))

(defun fold (in low high)
  "Folds the in value between low and high"
    (cond ((>= in high)
           (let ((in1 (- (+ high high) in)))
             (when (>= in1 low) (return-from fold in1))))
          ((< in low)
           (let ((in1 (- (+ low low) in)))
             (when (< in1 high)
               (return-from fold in1))))
          (t (return-from fold in)))
  (let* ((x (- in low))
         (range (- high low))
         (range2 (+ range range))
         (c (- x (* range2 (floor (/ x range2))))))
    (when (>= c range)
      (setf c (- range2 c)))
    (+ c low)))

(defun wrap (in low high)
  (let ((range (- high low)))
    (cond ((>= in high)
         (let ((in1 (- in range)))
           (when (< in1 high)
                 (return-from wrap in1))))
        ((< in low)
         (let* ((in1 (+ in range)))
           (when (>= in1 low)
             (return-from wrap in1))))
        (t (return-from wrap in)))
  (when (= high low) (return-from wrap in))
  (- in (* range (floor (/ (- in low) range))))))
 
