(cl:defpackage #:symbol-namespaces
  (:nicknames #:symbol-namespace
              #:symspaces
              #:symspace)
  (:use #:cl)
  (:shadow #:find-package
           #:find-symbol)
  (:export #:namespace
           #:packages-mixin
           #:packages-identity-mixin
           #:packages-name-mixin
           #:standard-namespace
           #:define
           #:locate

           #:intern-package
           #:find-package
           #:make-package-name

           #:intern-symbol
           #:find-symbol))
