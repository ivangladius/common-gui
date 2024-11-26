
(defsystem "hello-world"
  :version "0.0.1"
  :author "Maximilian Ivan Filipov"
  :license ""
  :depends-on ("cffi" "str" "bordeaux-threads" "cffi-libffi")
  :components ((:module "src"
                :components
                ((:file "unix-utils")
                 (:file "safe-queue")
                 (:file "raylib-manager")
                 (:file "raylib-bindings")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "hello-world/tests"))))

(defsystem "hello-world/tests"
  :author ""
  :license ""
  :depends-on ("hello-world"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hello-world"
  :perform (test-op (op c) (symbol-call :rove :run c)))
