;;;; wave.lisp
;;;; TODO proper headers everywhere

(in-package #:waveflow)

;;; FIND-WAVE and (SETF FIND-WAVE)

(defvar *waves* (make-hash-table))

(defun find-wave (name)
  (values (gethash name *waves*)))

(defun (setf find-wave) (new-value name)
  (check-type new-value (or null wave))
  (if new-value
      (setf (gethash name *waves*) new-value)
      (remhash name *waves*))
  new-value)

;;; WAVE

(defclass wave ()
  ((%name :accessor name
          :initarg :name)
   (%description :accessor description
                 :initarg :description))
  (:default-initargs :name nil
                     :description nil))

(defmethod initialize-instance :after ((wave wave) &key)
  (check-type (name wave) symbol)
  ;; (when (nth-value 1 (gethash (name wave) *waves*))
  ;;   (warn "Redefining wave ~S" (name wave)))
  ;; (setf (gethash (name wave) *waves*) wave)
  )

(defmethod print-object ((object wave) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((name name) (description description)) object
      (if name
          (princ name stream)
          (princ "(anonymous)" stream))
      (when description (format stream " (~A)" description)))))

(defparameter *wave-format* "~@[~A~] ~A~@[ for ~A~]: ~A.~%")

(defgeneric logger (wave severity format-control &rest format-args)
  (:method ((wave wave) severity format-control &rest format-args)
    (let* ((severity (format nil "[~8A] " severity))
           (format-control (concatenate 'string severity format-control)))
      (apply #'format t format-control format-args))))

(defgeneric execute-wave (wave &rest args)
  (:documentation "Returns two values: the first is true if execution was
successful, otherwise is false. If execution was successful, then the second
value contains data returned from the wave; if not, it contains debugging
information, such as errors that were signaled.")
  (:method ((wave symbol) &rest args)
    (apply #'execute-wave (find-wave wave) args))
  (:method ((wave wave) &rest args)
    (warn "EXECUTE-WAVE, default method: wave ~S~@[ and args ~S~]." wave args))
  (:method :around ((wave wave) &rest args)
    (flet ((handler (e)
             (logger wave :error *wave-format*
                     (description wave) (name wave) (first args) e)
             (error (make-condition 'wave-failure :wave wave :reason e))))
      (handler-bind ((error #'handler))
        (call-next-method)))))

;;; WAVE-FAILURE

(define-condition wave-failure (error) ;; TODO waveflow-error
  ((%wave :reader wave
          :initarg :wave)
   (%reason :reader reason
            :initarg :reason))
  (:default-initargs :wave (error "Must provide WAVE.")
                     :reason nil)
  (:report (lambda (condition stream)
             (format stream "Failed to execute wave ~A.~@[~%Reason: ~A~]"
                     (wave condition) (reason condition)))))

;; (defmethod print-object ((object wave-failure) stream)
;;   (if *print-escape*
;;       (call-next-method)
;;       (print-unreadable-object (object stream :type t)
;;         (with-accessors ((name name) (description description)) (wave object)
;;           (if name
;;               (princ name stream)
;;               (princ "(anonymous)" stream))
;;           (when description (format stream " (~A)" description))))))

;;; CALLBACK-WAVE

(defclass callback-wave (wave)
  ((%callback :accessor callback
              :initarg :callback))
  (:default-initargs :callback (constantly nil)))

(defmethod execute-wave ((wave callback-wave) &rest args)
  (apply (callback wave) wave args))

;;; RETRY-WAVE

(defclass retry-wave (callback-wave)
  ((%retry-count :accessor retry-count
                 :initarg :retry-count)
   (%retry-fn :accessor retry-fn
              :initarg :retry-fn))
  (:default-initargs :retry-count 1
                     :retry-fn (constantly nil)))

(defparameter *retry-wave-failure-string*
  "Failed to execute wave ~A~@[ after ~D retries~].~@[~%Reason: ~A~]")

(define-condition retry-wave-failure (wave-failure) ()
  (:report (lambda (condition stream)
             (format stream *retry-wave-failure-string*
                     (wave condition) (retry-count (wave condition))
                     (reason condition)))))

(defmethod execute-wave ((wave retry-wave) &rest args)
  (let ((count (1- (retry-count wave))))
    (tagbody start
       (flet ((handler (e)
                (cond ((non-negative-integer-p (decf count))
                       (apply (retry-fn wave) wave args)
                       (go start))
                      (t (error (make-instance
                                 'retry-wave-failure
                                 :wave wave :reason e))))))
         (handler-bind ((error #'handler))
           (call-next-method))))))

;;; WRAPPED-WAVE

(defclass wrapped-wave (wave)
  ((%before-fn :accessor before-fn
               :initarg :before-fn)
   (%after-fn :accessor after-fn
              :initarg :after-fn))
  (:default-initargs :before-fn (constantly nil)
                     :after-fn (constantly nil)))

(defparameter *wrapped-wave-before-failure*
  "Failed to execute wrapped wave ~A during before-function call.~
~@[~%Reason: ~A~]")

(defparameter *wrapped-wave-during-failure*
  "Failed to execute wrapped wave ~A during next method call.
Before-function results: ~S~
~@[~%Reason: ~A~]")

(defparameter *wrapped-wave-after-failure*
  "Failed to execute wrapped wave ~A during after-function call.
Before-function results: ~S
Method call results: ~S~
~@[~%Reason: ~A~]")

(defun wrapped-wave-failure-report (condition stream)
  (case (state condition)
    (:before (format stream *wrapped-wave-before-failure*
                     (wave condition) (reason condition)))
    (:during (format stream *wrapped-wave-before-failure*
                     (wave condition) (before-result condition)
                     (reason condition)))
    (:after (format stream *wrapped-wave-after-failure*
                    (wave condition) (before-result condition)
                    (during-result condition) (reason condition)))))

(define-condition wrapped-wave-failure (wave-failure)
  ((%state :reader state
           :initarg :state)
   (%before-result :reader before-result
                   :initarg :before-result)
   (%during-result :reader during-result
                   :initarg :during-result))
  (:default-initargs :state (error "Must provide STATE.")
                     :before-result nil :during-result nil)
  (:report wrapped-wave-failure-report))

(defmethod execute-wave :around ((wave wrapped-wave) &rest args)
  (let ((state :before) before-result during-result)
    (flet ((handler (e)
             (error (make-condition 'wrapped-wave-failure
                                    :wave wave :state state :reason e
                                    :before-result before-result
                                    :during-result during-result))))
      (handler-bind ((error #'handler))
        (setf before-result (apply (before-fn wave) args))
        (setf state :during)
        (setf during-result (call-next-method))
        (setf state :after)
        (apply (after-fn wave) args)
        during-result))))
