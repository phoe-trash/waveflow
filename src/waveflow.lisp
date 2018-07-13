;;;; waveflow.lisp

(in-package #:waveflow)

(defmacro define-wave (name (class) &body options)
  (check-type name symbol)
  (check-type class symbol)
  (assert (subtypep class 'wave))
  `(make-instance ',class :name ',name
                  ,@(remove-from-plist options :name)))

(defmacro define-flow (name (class) (&rest waves) &body options)
  (check-type name symbol)
  (check-type class symbol)
  (assert (subtypep class 'flow))
  (loop for wave in waves do (assert (every #'symbolp wave)))
  (assert (every (lambda (x) (count '-> x :test #'string=)) waves)
          (waves) "Invalid wave list.")
  `(make-instance ',class :name ',name :waves ',(process-waves waves)
                  ,@(remove-from-plist options :name :waves)))

(defun process-waves (lists)
  (loop for list in lists
        for dependencies = (subseq list 0 (position '-> list :test #'string=))
        for dependents = (subseq list (1+ (position '-> list :test #'string=)))
        nconc (loop for dependency in dependencies
                    nconc (loop for dependent in dependents
                                collect (list dependency dependent)))))

(define-wave foo (executable-wave))

(define-wave bar (executable-wave))

(define-wave baz (executable-wave))

(define-wave quux (executable-wave))

(define-flow foo (flow)
    ((foo -> bar baz)
     (bar baz -> quux)))
