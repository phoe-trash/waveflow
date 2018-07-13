;;;; package.lisp

(defpackage #:waveflow
  (:use #:cl
        #:alexandria
        #:phoe-toolbox)
  (:export
   ;; FIND-WAVE
   #:*waves* #:find-wave
   ;; WAVE
   #:wave #:name #:description #:logger #:execute-wave #:error-fn
   ;; CALLBACK WAVE
   #:callback-wave #:callback
   ;; RETRY-WAVE
   #:retry-wave #:retry-count #:retry-fn
   ;; WRAPPED-WAVE
   #:wrapped-wave #:before-fn #:after-fn
   ;; PUSH-WAVE
   #:push-wave #:load-fn #:push-fn
   ;; PULL-WAVE
   #:pull-wave #:pull-fn #:save-fn
   ;; FIND-FLOW
   #:*flows* #:find-flow
   ;; FLOW
   #:flow #:name #:spawn-fn #:waves #:execute-flow
   ))
