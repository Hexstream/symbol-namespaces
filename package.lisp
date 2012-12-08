(cl:defpackage #:symbol-namespaces
  (:nicknames #:symbol-namespace
              #:symspaces
              #:symspace)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:shadow #:find-package
           #:find-symbol)
  (:export #:define
           #:ensure
           #:locate
           #:name
           #:name-mixin

           #:namespace
           #:intern-symbol
           #:find-symbol

           #:packages
           #:intern-package
           #:find-package
           #:make-package-name
           #:packages-identity-mixin
           #:packages-name-mixin

           #:standard-namespace))
