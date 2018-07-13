;;;; wave.lisp

(in-package #:waveflow)

(export '(async-wave async-callback-wave async-flow) '#:waveflow)

;;;; ASYNC-WAVE

(defclass async-wave (executable-wave) ())

(defmethod compute-execution-status ((wave async-wave) dependencies)
  (if (not (boundp '*current-flow*))
      (progn (warn "ASYNC-WAVE called outside of ASYNC-FLOW.")
             (call-next-method))
      (bt:with-lock-held ((lock *current-flow*))
        (call-next-method))))

(defmethod after-execution ((wave async-wave) dependencies)
  (if (not (boundp '*current-flow*))
      (progn (warn "ASYNC-WAVE called outside of ASYNC-FLOW.")
             (call-next-method))
      (bt:with-lock-held ((lock *current-flow*))
        (call-next-method))))

(defclass async-callback-wave (async-wave callback-wave) ())

;;;; ASYNC-FLOW

(defun async-flow-spawn (wave &rest args)
  (let ((fn (curry #'apply #'execute-wave wave args))
        (initial-bindings `((*current-flow* . ,*current-flow*)
                            (*executing-waves* . ,*executing-waves*)
                            (*executed-waves* . ,*executed-waves*)
                            (*waves* . ,*waves*)
                            (*flows* . ,*flows*)))
        (name (format nil "Waveflow: ~A" (description wave))))
    (bt:make-thread fn :name name :initial-bindings initial-bindings)))

(defclass async-flow (flow)
  ((%lock :accessor lock
          :initarg :lock))
  (:default-initargs :lock (bt:make-lock) :spawn-fn #'async-flow-spawn))

(defmethod execute-flow :around ((flow async-flow) &rest args)
  (declare (ignore args))
  (let ((*executed-waves* (make-hash-table)))
    (call-next-method)))

;;; TODO HANDLED-ASYNC-FLOW
