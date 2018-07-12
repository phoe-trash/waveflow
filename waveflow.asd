;;;; waveflow.asd

(asdf:defsystem #:waveflow
  :description "Task orchestration library for Common Lisp"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "BSD 2-clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:phoe-toolbox
               #:named-readtables
               #:protest/test-case
               #:protest/1am)
  :components ((:file "src/package")
               (:file "src/wave")
               ;; (:file "src/flow")
               ;; (:file "src/waveflow")
               ))
