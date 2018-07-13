;;;; package.lisp

(defpackage #:waveflow
  (:use #:cl
        #:alexandria
        #:phoe-toolbox)
  (:export
   #:*waves*
   #:*flows*
   #:wave
   #:name
   #:description
   #:logged-wave
   #:logger
   #:executable-wave
   #:execute-wave
   #:callback-wave
   #:handled-wave
   #:network-wave
   #:handled-network-wave
   #:find-wave
   #:flow
   #:spawn-fn
   #:waves
   #:execute-flow))
