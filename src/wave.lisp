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
                 :initarg :description)
   (%error-fn :accessor error-fn
              :initarg :error-fn))
  (:default-initargs :name nil
                     :description nil
                     :error-fn (lambda (wave condition &rest args)
                                 (declare (ignore wave args))
                                 (values nil condition))))

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
information, such as errors that were signaled."))

(defmethod execute-wave ((wave symbol) &rest args)
  (apply #'execute-wave (find-wave wave) args))

(defmethod execute-wave ((wave wave) &rest args)
  (warn "Default EXECUTE-WAVE method called on wave ~S~@[ and args ~S~]."
        (name wave) args)
  (values t nil))

(defvar *executed-waves*)

(defvar *executing-waves*)

(defgeneric compute-execution-status (wave dependencies)
  (:documentation "Returns if the wave should be executed now based on whether
its dependencies have been executed."))

(defmethod compute-execution-status ((wave wave) dependencies)
  (dolist (dependency dependencies)
    (unless (gethash dependency *executed-waves*)
      (return-from compute-execution-status nil)))
  (when (or (gethash (name wave) *executing-waves*)
            (gethash (name wave) *executed-waves*))
    (return-from compute-execution-status nil))
  (setf (gethash (name wave) *executing-waves*) t)
  t)

(defgeneric after-execution (wave dependencies)
  (:documentation "Side effects after wave execution."))

(defmethod after-execution ((wave wave) dependencies)
  (declare (ignore dependencies))
  (setf (gethash (name wave) *executed-waves*) t))

(defmethod execute-wave :around ((wave wave) &rest args)
  (handler-case (call-next-method)
    (error (e)
      (logger wave :error *wave-format*
              (description wave) (name wave) (first args) e)
      (funcall (error-fn wave) wave e args))))

;;; CALLBACK-WAVE

(defclass callback-wave (executable-wave)
  ((%callback :accessor callback
              :initarg :callback))
  (:default-initargs :callback (constantly* t t)))

(defmethod execute-wave ((wave callback-wave) &rest args)
  (apply (callback wave) wave args))
