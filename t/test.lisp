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
  2 "Assign the wave via SETF FIND-WAVE."
  3 "Assert that it is retrieved via FIND-WAVE."
  4 "Delete the wave via SETF FIND-WAVE."
  5 "Assert that it is not retrieved via FIND-WAVE.")

(define-test test-wave
  (with-clean-state
    (let* ((name (gensym)) (description "asdf") (error-fn (constantly* t nil))
           (wave #1?(make-instance 'wave :name name
                                         :description description
                                         :error-fn error-fn)))
      (is (eq (find-wave name) nil))
      (setf (find-wave name) wave)
      (is (eq (find-wave name) wave))
      (is (eq (name wave) name))
      (is (string= (description wave) description))
      (is (equal (multiple-value-list (funcall (error-fn wave))) '(t nil)))
      (let ((string (with-output-to-string (*standard-output*)
                      (logger wave :debug "~D ~A ~S" 20 :foo :foo))))
        (is (string= string "[DEBUG   ] 20 FOO :FOO")))
      (signals warning (execute-wave wave))
      (setf (find-wave name) nil)
      (is (null (find-wave name))))))

;;; TEST-CALLBACK-WAVE
