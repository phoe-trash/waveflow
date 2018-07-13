;;;; waveflow.lisp

(in-package #:waveflow)

(defmacro define-wave (name (&optional (class 'callback-wave)) &body options)
  (check-type name symbol)
  (check-type class symbol)
  (assert (subtypep class 'wave))
  `(make-instance ',class :name ',name
                  ,@(remove-from-plist options :name)))

(defmacro define-flow
    (name (&optional (class 'flow)) (&rest options) &body waves)
  (check-type name symbol)
  (check-type class symbol)
  (assert (subtypep class 'flow))
  (dolist (wave waves) (assert (every #'symbolp wave)))
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

;; (defun simple-callback (wave &rest args)
;;   (format t "Wave ~S called with ~S~%" wave args)
;;   (values t args))

;; (define-wave foo (callback-wave)
;;   :callback 'simple-callback)

;; (define-wave bar (callback-wave)
;;   :callback 'simple-callback)

;; (define-wave baz (callback-wave)
;;   :callback 'simple-callback)

;; (define-wave quux (callback-wave)
;;   :callback 'simple-callback)

;; (define-flow foo (flow) ()
;;   (foo -> bar baz)
;;   (bar baz -> quux))
