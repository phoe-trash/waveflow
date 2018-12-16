;;;; bag.lisp

(in-package :waveflow)

;;; PETRI-NET

(defclass petri-net ()
  ((%places :accessor places
            :initarg :places
            :initform '())
   (%transitions :accessor transitions
                 :initarg :transitions
                 :initform '())
   (%population-policy :accessor population-policy
                       :initarg :population-policy)
   (%execution-policy :accessor execution-policy
                      :initarg :execution-policy)
   (%error-on-lockup-p :accessor error-on-lockup-p
                       :initarg :error-on-lockup-p)
   (%error-on-wave-failure-p :accessor error-on-wave-failure-p
                             :initarg :error-on-wave-failure-p)
   (%allow-transitions-without-inputs-p
    :accessor allow-transitions-without-inputs-p
    :initarg :allow-transitions-without-inputs-p))
  (:default-initargs
   :population-policy '(:all) :execution-policy '(:deterministic)
   :error-on-lockup-p t :error-on-wave-failure-p t
   :allow-transitions-without-inputs-p nil)
  (:metaclass funcallable-standard-class) #| TODO |#)

;;; SPECS-TO-EDGES

(defun group-spec (spec)
  (loop with stack = '()
        for thing in spec
        if (and (symbolp thing) (member thing '(-> => ~>) :test #'string=))
          collect (nreversef stack) into result
          and collect thing into result
          and do (setf stack '())
        else
          do (push thing stack)
        finally (return (append result (list (nreversef stack))))))

(defun split-spec (spec)
  (uiop:nest
   (loop for (lefts mid rights) on (group-spec spec) by #'cddr nconc)
   (loop for left in lefts nconc)
   (loop for right in rights collect (list left mid right))))

(defun specs-to-edges (specs)
  (let ((processed-specs (mapcan #'split-spec specs)))
    (loop for spec in processed-specs
          if (symbolp (car spec))
            collect spec into places
          else if (typep (car spec) '(cons (eql function) (cons symbol null)))
                 collect spec into transitions
          else do (error "Unknown spec: ~A" spec)
          finally (return (values places transitions)))))

;;; EXECUTE

(defmethod initialize-instance :after
    ((petri-net petri-net) &key specs places transitions)
  (set-funcallable-instance-function petri-net (petri-net-funcall petri-net))
  (when specs
    (when places (error "Can't provide both SPECS and PLACES."))
    (when transitions (error "Can't provide both SPECS and TRANSITIONS."))
    (multiple-value-bind (places transitions) (specs-to-edges specs)
      (setf (places petri-net) places
            (transitions petri-net) transitions))))

(defun petri-net-funcall (petri-net)
  (named-lambda petri-net-funcall (&rest args)
    (apply #'execute petri-net args)))

(defgeneric execute (petri-net &rest args)
  (:method ((petri-net petri-net) &rest args)
    ))

(defgeneric compute-starting-places (petri-net)
  (:method ((petri-net petri-net))
    (let* ((hash-table (make-hash-table))
           (starts (mapcar #'first (places petri-net)))
           (ends (mapcar #'third (transitions petri-net))))
      (dolist (start starts) (setf (gethash start hash-table) t))
      (dolist (end ends) (remhash end hash-table))
      (hash-table-keys hash-table))))

(defclass petri-net-executor ()
  ((%starting-points :reader starting-points
                     :initarg :starting-points)
   (%ending-points :reader ending-points
                   :initarg :ending-points))
  (:default-initargs :starting-points '()
                     :ending-points '()))


#|
(make-instance 'petri-net
               :specs '((a -> #'foo #'bar)
                        (#'foo #'bar -> b)
                        (#'bar -> c)
                        (b b c -> #'baz => d -> #'quux -> e)
                        (#'quux ~> f)))
|#

;;; ASYNC-PETRI-NET

;; (defclass async-petri-net (petri-net)
;;   ((%lock :accessor lock
;;           :initarg :lock))
;;   (:default-initargs :lock (bt:make-lock)))
