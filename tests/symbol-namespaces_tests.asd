(asdf:defsystem #:symbol-namespaces_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "symbol-namespaces unit tests."

  :depends-on ("symbol-namespaces"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:symbol-namespaces_tests)))
