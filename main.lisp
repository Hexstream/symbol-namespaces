(in-package #:symbol-namespaces)

(defvar *namespaces* (make-hash-table :test 'eq))


(defclass symspace:namespace () ())

(defclass symspace:packages-mixin ()
  ())

(defgeneric symspace:intern-symbol (symbol symbol-namespace)
  (:argument-precedence-order symbol-namespace symbol)
  (:method ((symbol symbol) (symbol-namespace packages-mixin))
    (let ((package (symbol-package symbol)))
      (if package
          (intern (symbol-name symbol)
                  (intern-package package symbol-namespace))
          (error "Don't know how to intern ~S in ~S ~
                  as the symbol has no home package."
                 symbol symbol-namespace)))))

(defgeneric symspace:find-symbol (symbol symbol-namespace)
  (:argument-precedence-order symbol-namespace symbol)
  (:method ((symbol symbol) (symbol-namespace packages-mixin))
    (let ((package (symbol-package symbol)))
      (if package
          (let ((managed-package (find-package package symbol-namespace)))
            (and managed-package
                 (cl:find-symbol (symbol-name symbol)
                                 managed-package)))
          (error "Don't know how to intern ~S in ~S ~
                  as the symbol has no home package."
                 symbol symbol-namespace)))))


(defclass symspace:packages-identity-mixin (symspace:packages-mixin)
  ((%packages :reader %packages
              :initform (make-hash-table :test 'eq))))

(defclass symspace:packages-name-mixin (symspace:packages-mixin)
  ((%packages :reader %packages
              :initform (make-hash-table :test 'equal))))

(defclass symspace:standard-namespace (symspace:packages-identity-mixin
                                       symspace:namespace)
  ())

(defgeneric symspace:intern-package (package symbol-namespace)
  (:argument-precedence-order symbol-namespace package)
  (:method ((package package) (symbol-namespace symspace:namespace))
    (or (symspace:find-package package symbol-namespace)
        (setf (symspace:find-package package symbol-namespace)
              (make-package (symspace:make-package-name package
                                                        symbol-namespace))))))

(defgeneric symspace:find-package (package symbol-namespace)
  (:argument-precedence-order symbol-namespace package)
  (:method ((package package) (symbol-namespace symspace:packages-identity-mixin))
    (identity (gethash package (%packages symbol-namespace))))
  (:method ((package package) (symbol-namespace symspace:packages-name-mixin))
    (let ((package-name (package-name package)))
      (if package-name
          (identity (gethash package-name (%packages symbol-namespace)))
          (error "Package ~S has been deleted and thus has no name."
                 package)))))

(defgeneric (setf symspace:find-package) (new package symbol-namespace)
  (:argument-precedence-order new symbol-namespace package)
  (:method (new (package package) (symbol-namespace symspace:packages-identity-mixin))
    (setf (gethash package (%packages symbol-namespace)) new))
  (:method (new (package package) (symbol-namespace symspace:packages-name-mixin))
    (let ((package-name (package-name package)))
      (if package-name
          (setf (gethash package-name (%packages symbol-namespace)) new)
          (error "Package ~S has been deleted and thus has no name."
                 package)))))

(defvar *counter* 0)

(defgeneric symspace:make-package-name (package symbol-namespace)
  (:argument-precedence-order symbol-namespace package)
  (:method ((package package) (symbol-namespace symspace:namespace))
    (format nil "TEST-~A" (incf *counter*))))


(defmacro symspace:define (name &rest options)
  (declare (ignore options))
  (check-type name symbol)
  `(setf (symspace:locate ',name)
         (make-instance 'symspace:standard-namespace :name ',name)))

(defun symspace:locate (symbol &key (errorp t))
  (check-type symbol symbol)
  (or (gethash symbol *namespaces*)
      (when errorp
        (error "There is no ~S with name ~S."
               'symspace:namespace symbol))))

(defun (setf symspace:locate) (new symbol &key (errorp t))
  (declare (ignore errorp))
  (check-type symbol symbol)
  (check-type new symspace:namespace)
  (setf (gethash symbol *namespaces*)
        new))
