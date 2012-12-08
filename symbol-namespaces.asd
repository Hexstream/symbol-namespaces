(asdf:defsystem #:symbol-namespaces

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Defines a new kind of package that's named by a symbol rather than a string and that maps from existing symbols to their respective \"implicitly managed\" counterparts. The motivating use-case is to conceptually allow multiple definitions of the same kind on a single symbol, without conflicts."

  :depends-on (#:map-bind)

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
