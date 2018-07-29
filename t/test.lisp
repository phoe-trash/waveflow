;;;; test.lisp

(defpackage #:waveflow/test
  (:use #:cl
        #:alexandria
        #:waveflow
        #:protest/1am
        #:phoe-toolbox
        #:named-readtables)
  (:import-from #:protest/test-case
                #:define-test-case))

(in-package #:waveflow/test)
(in-readtable protest/1am)

(defmacro with-clean-state (&body body)
  `(let ((*waves* (make-hash-table))
         ;; (*flows* (make-hash-table))
         )
     ,@body))

(defmacro with-silence (&body body)
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
     ,@body))

;;; TEST-WAVE

(define-test-case test-wave ()
  1 "Create a new wave."
  2 "Assert that the wave is not retrievable via FIND-WAVE."
  3 "Assign the wave via SETF FIND-WAVE."
  4 "Assert that it is retrieved via FIND-WAVE."
  5 "Assert that the wave's name is set."
  6 "Assert that the wave's description is set."
  7 "Assert that the wave's default logger works."
  8 "Assert that the wave's execution signals a warning."
  9 "Delete the wave via SETF FIND-WAVE."
  10 "Assert that it is not retrieved via FIND-WAVE.")

(define-test test-wave
  (with-clean-state
    (let* ((name (gensym)) (description "asdf")
           (wave #1?(make-instance 'wave :name name
                                         :description description)))
      #2?(is (eq (find-wave name) nil))
      #3?(setf (find-wave name) wave)
      #4?(is (eq (find-wave name) wave))
      #5?(is (eq (name wave) name))
      #6?(is (string= (description wave) description))
      #7?(let ((string (with-output-to-string (*standard-output*)
                         (logger wave :debug "~D ~A ~S" 20 :foo :foo))))
           (is (string= string "[DEBUG   ] 20 FOO :FOO")))
      #8?(signals warning (execute-wave wave))
      #9?(setf (find-wave name) nil)
      #10?(is (null (find-wave name))))))

;;; TEST-CALLBACK-WAVE

(define-test-case test-callback-wave ()
  1 "Create a callback wave."
  2 "Execute the wave."
  3 "Assert that the wave's callback has been executed.")

(define-test test-callback-wave
  (with-clean-state
    (let* ((result nil)
           (callback (lambda (&rest args) (declare (ignore args))
                       (setf result t)))
           (wave #1?(make-instance 'callback-wave :callback callback)))
      #2?(execute-wave wave)
      #3?(is (eq result t)))))

(define-test-case test-callback-wave-negative ()
  1 "Create a callback wave with an erroring callback."
  2 "Execute the wave."
  3 "Assert that the wave signaled a wave failure error.")

(define-test test-callback-wave-negative
  (with-clean-state
    (with-silence
      (let* ((callback (lambda (&rest args) (declare (ignore args))
                         (error "Test failure")))
             (wave #1?(make-instance 'callback-wave :callback callback)))
        #3?(signals wave-failure #2?(execute-wave wave))))))

;;; TEST-RETRY-WAVE

(define-test-case test-retry-wave ()
  1 "Create a retry wave with five retry attempts and a callback that ~
     succeeds on the fifth retry."
  2 "Execute the wave."
  3 "Assert that the retry function has been called five times.")

(define-test test-retry-wave
  (with-clean-state
    (with-silence
      (let* ((result nil) (count 0)
             (callback (lambda (&rest args) (declare (ignore args))
                         (push t result)
                         (unless (= 5 (incf count))
                           (error "Callback failure"))))
             (wave #1?(make-instance 'retry-wave :callback callback
                                                 :retry-count 5)))
        #2?(execute-wave wave)
        #3?(is (and (= (length result) 5)
                    (every (curry #'eq t) result)))))))

(define-test-case test-retry-wave-negative ()
  1 "Create a retry wave with five retry attempts and a failing callback."
  2 "Execute the wave."
  3 "Assert that executing the wave fails."
  4 "Assert that the retry function has been called five times.")

(define-test test-retry-wave-negative
  (with-clean-state
    (with-silence
      (let* ((result nil)
             (callback (lambda (&rest args) (declare (ignore args))
                         (push t result) (error "Callback failure")))
             (wave #1?(make-instance 'retry-wave :callback callback
                                                 :retry-count 5)))
        #3?(signals wave-failure #2?(execute-wave wave))
        #4?(is (and (= (length result) 5)
                    (every (curry #'eq t) result)))))))

;;; TEST-WRAPPED-WAVE

(define-test-case test-wrapped-wave ()
  1 "Create a wrapped wave."
  2 "Execute the wave."
  3 "Assert that the before and after functions have been called in proper ~
     order.")

(define-test test-wrapped-wave
  (with-clean-state
    (let* ((result nil)
           (before-fn (lambda (&rest args) (declare (ignore args))
                        (push :before result)))
           (after-fn (lambda (&rest args) (declare (ignore args))
                       (push :after result)))
           (wave #1?(make-instance 'wrapped-wave :before-fn before-fn
                                                 :after-fn after-fn)))
      (handler-bind ((warning #'muffle-warning))
        (signals* warning 1 #2?(execute-wave wave)))
      #3?(is (equal result '(:after :before))))))

(define-test-case test-wrapped-wave-negative-1 ()
  1 "Create a wrapped wave with a failing before-function."
  2 "Execute the wave."
  3 "Assert that executing the wave fails."
  4 "Assert that the before function been called.")

(define-test test-wrapped-wave-negative-1
  (with-clean-state
    (with-silence
      (let* ((result nil)
             (before-fn (lambda (&rest args) (declare (ignore args))
                          (push :before result) (error "Failure")))
             (wave #1?(make-instance 'wrapped-wave :before-fn before-fn)))
        #3?(signals wave-failure #2?(execute-wave wave))
        #4?(is (equal result '(:before)))))))

(define-test-case test-wrapped-wave-negative-2 ()
  1 "Create a wrapped wave with a failing after-function."
  2 "Execute the wave."
  3 "Assert that executing the wave fails."
  4 "Assert that the after function been called.")

(define-test test-wrapped-wave-negative-2
  (with-clean-state
    (with-silence
      (let* ((result nil)
             (before-fn (lambda (&rest args) (declare (ignore args))
                          (push :before result)))
             (after-fn (lambda (&rest args) (declare (ignore args))
                         (push :after result) (error "Failure")))
             (wave #1?(make-instance 'wrapped-wave :before-fn before-fn
                                                   :after-fn after-fn)))
        #3?(signals wave-failure (handler-bind ((warning #'muffle-warning))
                                   #2?(execute-wave wave)))
        #3?(is (equal result '(:after :before)))))))

;;; TEST-PUSH-WAVE

(define-test-case test-push-wave ()
  1 "Create a push wave."
  2 "Execute the wave."
  3 "Assert that the before, load, push, and after functions have been called ~
     in proper order.")

(define-test test-push-wave
  (with-clean-state
    (let* ((result nil)
           (before-fn (lambda (&rest args) (declare (ignore args))
                        (push :before result)))
           (load-fn (lambda (&rest args) (declare (ignore args))
                      (push :load result)))
           (push-fn (lambda (&rest args) (declare (ignore args))
                      (push :push result)))
           (after-fn (lambda (&rest args) (declare (ignore args))
                       (push :after result)))
           (wave #1?(make-instance 'push-wave
                                   :before-fn before-fn :load-fn load-fn
                                   :push-fn push-fn :after-fn after-fn)))
      (handler-bind ((warning #'muffle-warning))
        #2?(execute-wave wave))
      #3?(is (equal result '(:after :push :load :before))))))
