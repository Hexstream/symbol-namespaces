(in-package #:symbol-namespaces)

(defvar *namespaces* (make-hash-table :test 'eq))


(defclass symspace:namespace () ())

(defgeneric symspace:name (namespace))

(defclass symspace:name-mixin ()
  ((%name :initarg :name
          :reader symspace:name
          :type symbol)))

(defmethod print-object ((mixin name-mixin) stream)
  (print-unreadable-object (mixin stream :type t)
    (prin1 (symspace:name mixin) stream)))

(defclass symspace:packages ()
  ())

(defgeneric symspace:intern-symbol (symbol symbol-namespace)
  (:argument-precedence-order symbol-namespace symbol)
  (:method ((symbol symbol) (symbol-namespace symspace:packages))
    (let ((package (symbol-package symbol)))
      (if package
          (identity (intern (symbol-name symbol)
                            (intern-package package symbol-namespace)))
          (error "Don't know how to intern ~S in ~S ~
                  as the symbol has no home package."
                 symbol symbol-namespace)))))

(defgeneric symspace:find-symbol (symbol symbol-namespace)
  (:argument-precedence-order symbol-namespace symbol)
  (:method ((symbol symbol) (symbol-namespace symspace:packages))
    (let ((package (symbol-package symbol)))
      (if package
          (let ((managed-package (find-package package symbol-namespace)))
            (and managed-package
                 (cl:find-symbol (symbol-name symbol)
                                 managed-package)))
          (error "Don't know how to intern ~S in ~S ~
                  as the symbol has no home package."
                 symbol symbol-namespace)))))


(defclass symspace:packages-identity-mixin (symspace:packages)
  ((%packages :reader %packages
              :initform (make-hash-table :test 'eq))))

(defclass symspace:packages-name-mixin (symspace:packages)
  ((%packages :reader %packages
              :initform (make-hash-table :test 'equal))))

(defclass symspace:standard-namespace (symspace:name-mixin
                                       symspace:packages-identity-mixin
                                       symspace:namespace)
  ())

(defgeneric symspace:intern-package (package symbol-namespace)
  (:argument-precedence-order symbol-namespace package)
  (:method ((package package) (symbol-namespace symspace:namespace))
    (or (symspace:find-package package symbol-namespace)
        (setf (symspace:find-package package symbol-namespace)
              (make-package (symspace:make-package-name package
                                                        symbol-namespace))))))

(defun %call-with-package-name (function package)
  (let ((package-name (package-name package)))
    (if package-name
        (funcall function package-name)
        (error "Package ~S has been deleted and thus has no name."
               package))))

(defgeneric symspace:find-package (package symbol-namespace)
  (:argument-precedence-order symbol-namespace package)
  (:method ((package package) (symbol-namespace symspace:packages-identity-mixin))
    (identity (gethash package (%packages symbol-namespace))))
  (:method ((package package) (symbol-namespace symspace:packages-name-mixin))
    (%call-with-package-name
     (lambda (package-name)
       (identity (gethash package-name (%packages symbol-namespace))))
     package)))

(defgeneric (setf symspace:find-package) (new package symbol-namespace)
  (:argument-precedence-order new symbol-namespace package)
  (:method (new (package package) (symbol-namespace symspace:packages-identity-mixin))
    (setf (gethash package (%packages symbol-namespace)) new))
  (:method (new (package package) (symbol-namespace symspace:packages-name-mixin))
    (%call-with-package-name
     (lambda (package-name)
       (setf (gethash package-name (%packages symbol-namespace)) new))
     package)))

(defgeneric symspace:make-package-name (package symbol-namespace)
  (:argument-precedence-order symbol-namespace package)
  (:method ((package package) (symbol-namespace symspace:namespace))
    (%call-with-package-name
     (lambda (package-name)
       (let ((namespace-name (symspace:name symbol-namespace)))
         (format nil "~A_+_~A_+_~A_+_~A"
                 '#:symspace
                 (%call-with-package-name #'identity
                                          (symbol-package namespace-name))
                 (symbol-name namespace-name)
                 package-name)))
     package)))

(defun symspace:locate (name &key (errorp t))
  (check-type name symbol)
  (or (gethash name *namespaces*)
      (when errorp
        (error "There is no ~S with name ~S."
               'symspace:namespace name))))

(defun (setf symspace:locate) (new name &key (errorp t))
  (declare (ignore errorp))
  (check-type name (and symbol (not null)))
  (check-type new symspace:namespace)
  (setf (gethash name *namespaces*)
        new))

(defun %remove-keys (keys plist)
  (do (acc
       (plist plist (cddr plist)))
      ((endp plist) (nreverse acc))
    (destructuring-bind (key value &rest rest) plist
      (declare (ignore rest))
      (unless (member key keys)
        (setf acc (list* value key acc))))))

(defun symspace:ensure (name &rest keys
                        &key (class 'symspace:standard-namespace)
                        &allow-other-keys)
  (setf keys (%remove-keys '(:class) keys))
  (let ((existing (symspace:locate name :errorp nil)))
    (cond (existing
           (setf class (etypecase class
                         (class class)
                         (symbol (find-class class))))
           (if (eq class (class-of existing))
               (apply #'reinitialize-instance existing keys)
               (apply #'change-class existing class keys)))
          (t (setf (symspace:locate name)
                   (make-instance class :name name))))))

(defmacro symspace:define (name &body options)
  (check-type name symbol)
  (let ((plist
         (let (classp)
           (mapcan (lambda (option)
                     (destructuring-bind (kind &rest values) option
                       (if (eq kind :class)
                           (destructuring-bind (supplied-class) values
                             (prog1 (list :class `',supplied-class)
                               (if classp
                                   (error "Duplicate :class option in ~S." options)
                                   (setf classp t))))
                           (list `',kind `',values))))
                   options))))
    `(symspace:ensure ',name ,@plist)))
