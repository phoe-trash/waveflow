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
  (:method ((wave executable-wave) &rest args)
    (warn "Default method for EXECUTE-WAVE called with args ~S." args)
    t))

;;; HANDLED-WAVE

(defclass handled-wave (logged-wave executable-wave) ())

(defmethod execute-wave :around ((wave handled-wave) &rest args)
  (handler-case (call-next-method)
    (error (e)
      (logger wave :error *wave-format*
              (class-name (class-of wave)) (name wave) (first args) e)
      nil)))

;;; NETWORK-WAVE

(defclass network-wave (logged-wave executable-wave)
  ((%fetch-fn :accessor fetch-fn
              :initarg :fetch-fn) ;; Returns data for MERGE and SPAWN
   (%merge-fn :accessor merge-fn
              :initarg :merge-fn) ;; Returns if any data was merged
   (%spawn-fn :accessor spawn-fn
              :initarg :spawn-fn) ;; Returns number of spawned tasks
   (%done-fn :accessor done-fn
             :initarg :done-fn))  ;; Return value is ignored
  (:default-initargs :fetch-fn (constantly nil) :merge-fn (constantly nil)
                     :spawn-fn (constantly 0) :done-fn (constantly nil)))

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
      (let ((n (apply (spawn-fn wave) wave data)))
        (when (/= n 0)
          (debug (format nil "~D tasks spawned" n))))
      (apply (done-fn wave) data)
      (debug "done")
      t)))

;;; HANDLED-NETWORK-WAVE

(defclass handled-network-wave (network-wave handled-wave) ())
