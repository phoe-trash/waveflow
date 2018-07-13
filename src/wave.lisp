;;;; wave.lisp

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
          :initarg :name))
  (:default-initargs :name (error "Must provide NAME.")))

(defmethod initialize-instance :after ((wave wave) &key)
  (check-type (name wave) symbol)
  (when (nth-value 1 (gethash (name wave) *waves*))
    (warn "Redefining wave ~S" (name wave)))
  (setf (gethash (name wave) *waves*) wave))

;;; LOGGED-WAVE

(defparameter *wave-format* "~@[~A~] ~A~@[ for ~A~]: ~A.~%")

(defclass logged-wave (wave) ())

(defgeneric logger (wave severity format-control &rest format-args)
  (:method ((wave logged-wave) severity format-control &rest format-args)
    (let* ((severity (format nil "[~8A] " severity))
           (format-control (concatenate 'string severity format-control)))
      (apply #'format t format-control format-args))))

;;; EXECUTABLE-WAVE

(defclass executable-wave (wave) ())

(defgeneric execute-wave (wave &rest args)
  (:documentation "Returns two values: the first is true if execution was
successful, otherwise is false. If execution was successful, then the second
value contains data returned from the wave."))
;; TODO rethink this; we have no means of fetching the values back from the wave

(defmethod execute-wave ((wave executable-wave) &rest args)
  (warn "Default method for EXECUTE-WAVE called with args ~S." args)
  (values t nil))

;;; CALLBACK-WAVE

(defclass callback-wave (executable-wave)
  ((%callback :accessor callback
              :initarg :callback))
  (:default-initargs
   :callback (lambda (&rest args) (declare (ignore args)) (values t t))))

(defmethod execute-wave ((wave callback-wave) &rest args)
  (apply (callback wave) wave args))

;;; HANDLED-WAVE

(defclass handled-wave (logged-wave executable-wave) ())

(defmethod execute-wave :around ((wave handled-wave) &rest args)
  (handler-case (call-next-method)
    (error (e)
      (logger wave :error *wave-format*
              (class-name (class-of wave)) (name wave) (first args) e)
      (values nil nil))))

;;; NETWORK-WAVE

(defclass network-wave (logged-wave executable-wave)
  ((%fetch-fn :accessor fetch-fn
              :initarg :fetch-fn) ;; Returns data for MERGE
   (%merge-fn :accessor merge-fn
              :initarg :merge-fn) ;; Returns if any data was merged
   (%done-fn :accessor done-fn
             :initarg :done-fn))  ;; Return value is ignored
  (:default-initargs :fetch-fn (constantly nil) :merge-fn (constantly nil)
                     :done-fn (constantly nil)))

(declaim (inline network-wave-debug))

(defun network-wave-debug (wave first-arg)
  (lambda (message)
    (logger wave :debug *wave-format* (class-name (class-of wave))
            (name wave) first-arg message)))

(defmethod execute-wave ((wave network-wave) &rest args)
  (fbind ((debug (network-wave-debug wave (first args))))
    (debug "begin")
    (let ((data (apply (fetch-fn wave) args)))
      (debug "fetched")
      (when (apply (merge-fn wave) data)
        (debug "merged"))
      (apply (done-fn wave) data)
      (debug "done")
      (values t data))))

;;; HANDLED-NETWORK-WAVE

(defclass handled-network-wave (network-wave handled-wave) ())
