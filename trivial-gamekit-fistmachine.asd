(asdf:defsystem :trivial-gamekit-fistmachine
  :description "Finite state machine for trivial-gamekit"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (trivial-docstring trivial-gamekit)
  :components ((:file "fistmachine")))


(asdf:defsystem trivial-gamekit-fistmachine/example
  :description "trivial-gamekit-fistmachine example"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (trivial-gamekit-fistmachine)
  :components ((:file "example")))
