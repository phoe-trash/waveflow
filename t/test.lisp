;;;; test.lisp

;;; TODO negative tests

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

;;; TEST-WAVE

(define-test-case test-wave () ;; TODO fix
  1 "Create a new wave."
  2 "Assert that the wave is not retrievable via FIND-WAVE."
  3 "Assign the wave via SETF FIND-WAVE."
  4 "Assert that it is retrieved via FIND-WAVE."
  5 "Assert that the wave's name is set."
  6 "Assert that the wave's description is set."
  7 "Assert that the wave's error function is set."
  8 "Assert that the wave's default logger works."
  9 "Assert that the wave's execution signals a warning."
  10 "Delete the wave via SETF FIND-WAVE."
  11 "Assert that it is not retrieved via FIND-WAVE.")

(define-test test-wave
  (with-clean-state
    (let* ((name (gensym)) (description "asdf") (error-fn (constantly* nil t))
           (wave #1?(make-instance 'wave :name name
                                         :description description
                                         :error-fn error-fn)))
      #2?(is (eq (find-wave name) nil))
      #3?(setf (find-wave name) wave)
      #4?(is (eq (find-wave name) wave))
      #5?(is (eq (name wave) name))
      #6?(is (string= (description wave) description))
      #7?(is (equal (multiple-value-list (funcall (error-fn wave))) '(nil t)))
      #8?(let ((string (with-output-to-string (*standard-output*)
                         (logger wave :debug "~D ~A ~S" 20 :foo :foo))))
           (is (string= string "[DEBUG   ] 20 FOO :FOO")))
      #9?(signals warning (execute-wave wave))
      #10?(setf (find-wave name) nil)
      #11?(is (null (find-wave name))))))

;;; TEST-CALLBACK-WAVE

(define-test-case test-callback-wave ()
  1 "Create a callback wave."
  2 "Execute the wave."
  3 "Assert that the wave's callback has been executed.")

(define-test test-callback-wave
  (with-clean-state
    (let* ((result nil)
           (callback (lambda (&rest args)
                       (declare (ignore args))
                       (setf result t)))
           (wave #1?(make-instance 'callback-wave :callback callback)))
      #2?(execute-wave wave)
      #3?(is (eq result t)))))
