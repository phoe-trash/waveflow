;;;; test.lisp

(defpackage #:waveflow/test
  (:use #:cl
        #:alexandria
        #:waveflow
        #:protest/1am
        #:named-readtables)
  (:import-from #:protest/test-case
                #:define-test-case))

(in-package #:waveflow/test)
(in-readtable protest/1am)

(defmacro with-clean-state (&body body)
  `(let ((*waves* (make-hash-table))
         (*flows* (make-hash-table)))
     ,@body))

;;; TEST-FIND-WAVE

(define-test-case test-find-wave ()
  1 "Create a new wave."
  2 "Assign the wave via SETF FIND-WAVE."
  3 "Assert that it is retrieved via FIND-WAVE."
  4 "Delete the wave via SETF FIND-WAVE."
  5 "Assert that it is not retrieved via FIND-WAVE.")

(define-test test-find-wave
  (with-clean-state
    (let ((wave #1?(make-instance 'wave :name '#1=#.(gensym))))
      #2?(setf (find-wave '#1#) wave)
      #3?(is (eq (find-wave '#1#) wave))
      #4?(setf (find-wave '#1#) nil)
      #5?(is (null (find-wave '#1#))))))

;;; TEST-MAKE-WAVE-ERROR

(define-test-case test-make-wave-error ()
  1 "Assert creating a wave without name is an error."
  2 "Assert creating a wave with non-symbol name is an error.")

(define-test test-make-wave-error
  (with-clean-state
    #1?(signals waveflow-error (make-instance 'wave))
    #2?(signals waveflow-error (make-instance 'wave :name 42))))

;;; TEST-MAKE-LOGGED-WAVE

(define-test-case test-make-logged-wave ()
  1 "Create a new logged wave."
  2 "Invoke the wave's logger."
  3 "Assert that the message has been logged.")

(define-test test-make-logged-wave
  (with-clean-state
    (let* ((wave #1?(make-instance 'logged-wave :name '#1=#.(gensym)))
           (string (with-output-to-string (*standard-output*)
                     #2?(logger wave :foo "~A BAZ" :bar))))
      #3?(is (string= string "[FOO     ] BAR BAZ")))))

;;; TEST-MAKE-EXECUTABLE-WAVE

(define-test-case test-make-executable-wave ()
  1 "Create a new executable wave."
  2 "Execute the wave."
  3 "Assert that the wave's default method has been executed.")

(define-test test-make-executable-wave
  (with-clean-state
    (let ((wave #1?(make-instance 'executable-wave :name '#1=#.(gensym))))
      #3?(signals warning #2?(execute-wave wave)))))

;;; TEST-MAKE-HANDLED-WAVE

(define-test-case test-make-handled-wave ()
  1 "Create a new handled erroring wave."
  2 "Execute the wave."
  3 "Assert NIL was returned.")

(defclass erroring-wave (handled-wave) ())

(defmethod execute-wave ((wave erroring-wave) &rest args)
  (declare (ignore args))
  (error ""))

(defmethod logger ((wave erroring-wave) severity format-control
                   &rest format-args)
  (declare (ignore wave severity format-control format-args)))

(define-test test-make-handled-wave
  (with-clean-state
    (let* ((result nil)
           (fn (lambda (&rest x) (declare (ignore x)) (setf result t)))
           (wave #1?(make-instance 'erroring-wave :name 'foo :error-fn fn)))
      (handler-bind ((warning #'muffle-warning))
        #2?(execute-wave wave)
        #3?(is result)))))

;;; TEST-MAKE-NETWORK-WAVE

(define-test-case test-make-network-wave ()
  1 "Create a new network wave."
  2 "Execute the wave."
  3 "Assert that all phases have been executed.")

(define-test test-make-network-wave
  (with-clean-state
    (macrolet ((fn (x &optional y) `(lambda (&rest args)
                                      (declare (ignore args))
                                      (push ,x result) ,y)))
      (let* ((result '())
             (expected '(:done-fn :merge-fn :fetch-fn))
             (wave #1?(make-instance 'network-wave :name '#1=#.(gensym)
                                     :fetch-fn (fn :fetch-fn)
                                     :merge-fn (fn :merge-fn)
                                     :done-fn (fn :done-fn))))
        #2?(let ((*standard-output* (make-broadcast-stream)))
             (is (eql t (execute-wave wave))))
        #3?(progn (is (null (set-difference result expected))))))))

;;; TEST-MAKE-FLOW

(define-test-case test-make-flow ()
  1 "Create a new flow with four waves."
  2 "Execute the flow."
  3 "Assert that the flow has been executed.")

(define-test test-make-flow
  (with-clean-state
    (let* ((result '())
           (waves '(foo bar baz quux))
           (expected (reverse waves))
           (edges '((foo bar) (bar quux) (foo baz) (baz quux))))
      (flet ((make-wave (thing)
               (make-instance 'callback-wave :name thing
                                             :callback (lambda (&rest args)
                                                         (push thing result)
                                                         (values t args)))))
        (mapc #'make-wave waves)
        (let ((flow #1?(make-instance 'flow :name 'frob :waves edges)))
          #2?(execute-flow flow)
          #3?(is (equal result expected)))))))

(define-test-case test-make-flow-fail-second-wave ()
  1 "Create a new flow with three waves, the second one being unsuccessful."
  2 "Execute the flow."
  3 "Assert that the third wave has not been executed.")

(define-test test-make-flow-fail-second-wave
  (with-clean-state
    (let* ((edges '((foo bar) (bar baz)))
           (error-callback (lambda (&rest args) (declare (ignore args))
                             (error "Should not reach this place."))))
      (labels ((make-fn (x y) (lambda (&rest args) (declare (ignore args))
                                (values x y)))
               (make-wave (name fn)
                 (make-instance 'callback-wave :name name :callback fn)))
        (make-wave 'foo (make-fn t nil))
        (make-wave 'bar (make-fn nil nil))
        (make-wave 'baz error-callback)
        (let ((flow #1?(make-instance 'flow :name 'frob :waves edges)))
          #3?(signals waveflow-error #2?(execute-flow flow)))))))
