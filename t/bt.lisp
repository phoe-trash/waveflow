;;;; bt.lisp

(in-package #:waveflow/test)
(in-readtable protest/1am)

(define-test-case test-make-async-flow ()
  1 "Create a new async flow with four waves."
  2 "Execute the async flow."
  3 "Assert that the async flow has been executed.")

(define-test test-make-async-flow
  (with-clean-state
    (let* ((result '())
           (lock (bt:make-lock))
           (waves '(foo bar baz quux))
           (edges '((foo bar) (bar quux) (foo baz) (baz quux)))
           (flow #1?(make-instance 'async-flow :name 'frob :waves edges)))
      (labels ((fn (thing)
                 (lambda (&rest args)
                   (sleep (random 0.01))
                   (bt:with-lock-held (lock) (push (print thing) result))
                   (values t args)))
               (make-wave (thing)
                 (make-instance 'async-callback-wave
                                :name thing :callback (fn thing)))
               (check-result ()
                 (and (= (length result) 4)
                      (eq (first result) 'quux)
                      (or (and (eq (second result) 'baz)
                               (eq (third result) 'bar))
                          (and (eq (second result) 'bar)
                               (eq (third result) 'baz)))
                      (eq (fourth result) 'foo))))
        (mapc #'make-wave waves)
        #2?(execute-flow flow)
        #3?(is (wait (1 0.01) (check-result)))))))
