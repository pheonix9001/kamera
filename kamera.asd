(in-package :asdf-user)

(defsystem "kamera"
  :version "0.1.0"
  :author "pheonix9001"
  :license ""
  :depends-on (alexandria iterate)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "An implementation of Kanren style logic programming, fitted to Common Lisp"
  :in-order-to ((test-op (test-op "kamera/tests"))))

(defsystem "kamera/tests"
  :author "pheonix9001"
  :license ""
  :depends-on ("kamera"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for kamera"
  :perform (test-op (op c) (symbol-call :rove :run c)))
