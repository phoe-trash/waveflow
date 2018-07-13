;;;; flow.lisp

(in-package #:waveflow)

;;; FIND-FLOW and (SETF FIND-FLOW)

(defvar *flows* (make-hash-table))

(defun find-flow (name)
  (values (gethash name *flows*)))

(defun (setf find-flow) (new-value name)
  (check-type new-value (or null flow))
  (if new-value
      (setf (gethash name *flows*) new-value)
      (remhash name *flows*))
  new-value)

;;; FLOW

(defclass flow ()
  ((%name :accessor name
          :initarg :name)
   (%spawn-fn :accessor spawn-fn
              :initarg :spawn-fn)
   (%waves :accessor waves
           :initarg :waves))
  (:default-initargs :name (error "Must provide NAME.")
                     :waves (error "Must provide WAVES.")
                     :spawn-fn #'execute-wave))

(defun wave-dependency-list-p (list)
  (and (listp list)
       (every (lambda (x) (= 2 (length x))) list)
       (setp list :test #'equal)
       (loop for (dependency dependent) in list
             unless (and (symbolp dependency) (symbolp dependent))
               return nil
             finally (return t))))

(defmethod initialize-instance :after ((flow flow) &key)
  (check-type (name flow) symbol)
  (check-type (spawn-fn flow) function)
  (assert (wave-dependency-list-p (waves flow)) ((waves flow))
          "Invalid wave dependency list: ~S" (waves flow))
  (multiple-value-bind (cyclicp symbol) (circular-graph-p (waves flow))
    (when cyclicp (error "Cycle detected for wave ~S." symbol)))
  (when (nth-value 1 (gethash (name flow) *flows*))
    (warn "Redefining flow ~S" (name flow)))
  (setf (gethash (name flow) *flows*) flow))

;;; EXECUTE-FLOW

(defvar *current-flow*)

(defvar *executed-waves*)

(defgeneric execute-flow (flow &rest args)
  (:documentation "Returns no meaningful value."))

(defmethod execute-flow :around ((flow flow) &rest args)
  (declare (ignore args))
  (let ((*current-flow* flow)
        (*executed-waves* (make-hash-table)))
    (call-next-method)
    (values)))

(defmethod execute-flow ((flow flow) &rest args)
  (let ((roots (mapcar #'find-wave (graph-roots (waves flow))))
        (spawn-fn (spawn-fn flow)))
    (loop for root in roots
          do (apply spawn-fn root args))))

(defun flow-dependencies-dependents (wave)
  (loop with name = (name wave)
        for (dependency dependent) in (waves *current-flow*)
        when (eq dependent name)
          collect dependency into dependencies
        when (eq dependency name)
          collect dependent into dependents
        finally (return (values dependencies dependents))))

(defmethod execute-wave :around ((wave executable-wave) &rest args)
  ;; TODO this will break for async - create ASYNC-WAVE
  (if (not (boundp '*current-flow*))
      (call-next-method)
      (multiple-value-bind (dependencies dependents)
          (flow-dependencies-dependents wave)
        (dolist (dependency dependencies)
          (unless (gethash dependency *executed-waves*)
            (return-from execute-wave)))
        (setf (gethash (name wave) *executed-waves*) t)
        (call-next-method)
        (prog1 (call-next-method)
          (loop with spawn-fn = (spawn-fn *current-flow*)
                for wave in (mapcar #'find-wave dependents)
                do (apply spawn-fn wave args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro define-flow (name options &body forms)
;;   (check-type name symbol)
;;   (let ((first-wave (caar forms))
;;         (prepare-fn (getf options :prepare-fn))
;;         (class (or (getf options :class) 'standard-flow))
;;         (next-waves (mapcar (curry #'remove '->) forms)))
;;     (with-gensyms (instance)
;;       `(let ((,instance (make-flow ',name ,prepare-fn
;;                                    ',first-wave ',next-waves ',class)))
;;          (setf (gethash ',name *flows*) ,instance)
;;          ',name))))

;; (define-flow synchronize (:prepare-fn 'synchronize-prepare)
;;   (login            -> download-account)
;;   (download-account -> download-furres
;;                     -> download-images)
;;   (download-furres  -> download-costumes
;;                     -> download-portraits
;;                     -> download-specitags))
