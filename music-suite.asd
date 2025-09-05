(defsystem "music-suite"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("alexandria"
               "transducers"
               "serapeum"
               "fset"
               "trivia"
               "mgl-pax")
  :components ((:module "src"
                :components
                ((:module "utils"
                 :components
                 ((:file "utils")))
                (:module "math"
                 :components
                 ((:file "random")))
                (:module "sequences"
                 :components
                 ((:file "random")))
                (:module "patterns"
                 :components
                 ((:file "patterns")
                  (:file "sequence"))))))
  :description ""
  :in-order-to ((test-op (test-op "music-suite/tests"))))

(defsystem "music-suite/tests"
  :author ""
  :license ""
  :depends-on ("music-suite"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for music-suite"
  :perform (test-op (op c) (symbol-call :rove :run c)))
