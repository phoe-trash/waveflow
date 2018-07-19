;;;; wave.lisp

;; (in-package #:waveflow)

;;; FIND-WAVE and (SETF FIND-WAVE)

;; (defvar *waves* (make-hash-table))

;; (defun find-wave (name)
;;   (values (gethash name *waves*)))

;; (defun (setf find-wave) (new-value name)
;;   (check-type new-value (or null wave))
;;   (if new-value
;;       (setf (gethash name *waves*) new-value)
;;       (remhash name *waves*))
;;   new-value)

;;; WAVE

;; (defclass wave ()
;;   ((%name :accessor name
;;           :initarg :name)
;;    (%description :accessor description
;;                  :initarg :description))
;;   (:default-initargs :name (waveflow-error "Must provide NAME.")
;;                      :description "Anonymous wave"))

;; (defmethod print-object ((object wave) stream)
;;   (print-unreadable-object (object stream :type t)
;;     (princ (name object) stream)))

(defmethod initialize-instance :after ((wave wave) &key)
  (check-type (name wave) symbol)
  (when (nth-value 1 (gethash (name wave) *waves*))
    (warn "Redefining wave ~S" (name wave)))
  (setf (gethash (name wave) *waves*) wave))

;;; LOGGED-WAVE

;; (defparameter *wave-format* "~@[~A~] ~A~@[ for ~A~]: ~A.~%")

;; (defclass logged-wave (wave) ())

;; (defgeneric logger (wave severity format-control &rest format-args)
;;   (:method ((wave logged-wave) severity format-control &rest format-args)
;;     (let* ((severity (format nil "[~8A] " severity))
;;            (format-control (concatenate 'string severity format-control)))
;;       (apply #'format t format-control format-args))))

;;; EXECUTABLE-WAVE

;; (defclass executable-wave (wave) ())

;; (defgeneric execute-wave (wave &rest args)
;;   (:documentation "Returns two values: the first is true if execution was
;; successful, otherwise is false. If execution was successful, then the second
;; value contains data returned from the wave; if not, it contains debugging
;; information, such as errors that were signaled."))

;; (defmethod execute-wave ((wave symbol) &rest args)
;;   (apply #'execute-wave (find-wave wave) args))

;; (defmethod execute-wave ((wave executable-wave) &rest args)
;;   (warn "Default method on EXECUTE-WAVE called on wave ~S~@[ and args ~S~]."
;;         (name wave) args)
;;   (values t nil))

;; (defvar *executed-waves*)

;; (defvar *executing-waves*)

;; (defgeneric compute-execution-status (wave dependencies)
;;   (:documentation "Returns if the wave should be "))

;; (defmethod compute-execution-status ((wave executable-wave) dependencies)
;;   (dolist (dependency dependencies)
;;     (unless (gethash dependency *executed-waves*)
;;       (return-from compute-execution-status nil)))
;;   (when (or (gethash (name wave) *executing-waves*)
;;             (gethash (name wave) *executed-waves*))
;;     (return-from compute-execution-status nil))
;;   (setf (gethash (name wave) *executing-waves*) t)
;;   t)

;; (defgeneric after-execution (wave dependencies)
;;   (:documentation "Side effects after wave execution."))

;; (defmethod after-execution ((wave executable-wave) dependencies)
;;   (declare (ignore dependencies))
;;   (setf (gethash (name wave) *executed-waves*) t))

;;; CALLBACK-WAVE

(defclass callback-wave (executable-wave)
  ((%callback :accessor callback
              :initarg :callback))
  (:default-initargs
   :callback (lambda (&rest args) (declare (ignore args)) (values t t))))

(defmethod execute-wave ((wave callback-wave) &rest args)
  (apply (callback wave) wave args))

;;; HANDLED-WAVE

;; (defclass handled-wave (logged-wave executable-wave)
;;   ((%error-fn :accessor error-fn
;;               :initarg :error-fn))
;;   (:default-initargs :error-fn (constantly nil)))

;; (defmethod execute-wave :around ((wave handled-wave) &rest args)
;;   (handler-case (call-next-method)
;;     (error (e)
;;       (logger wave :error *wave-format*
;;               (description wave) (name wave) (first args) e)
;;       (funcall (error-fn wave) e args)
;;       (values nil e))))

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
    (logger wave :debug *wave-format* (description wave)
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
