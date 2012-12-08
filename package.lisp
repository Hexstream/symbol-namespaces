(cl:defpackage #:symbol-namespaces
  (:nicknames #:symbol-namespace
              #:symspaces
              #:symspace)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:shadow #:find-package
           #:find-symbol)
  (:export #:namespace
           #:name-mixin
           #:name
           #:packages-mixin
           #:packages-identity-mixin
           #:packages-name-mixin
           #:standard-namespace

           #:intern-package
           #:find-package
           #:make-package-name

           #:intern-symbol
           #:find-symbol

           #:locate
           #:ensure
           #:define))
