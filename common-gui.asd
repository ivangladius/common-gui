
(defsystem "common-gui"
  :version "0.0.1"
  :author "Maximilian Ivan Filipov"
  :license ""
  :depends-on ("cffi" "str" "bordeaux-threads" "cffi-libffi" "local-time")
  :components ((:module "src"
                :components
                ((:file "unix-utils")
                 (:file "safe-queue")
                 (:file "safe-accessor")
                 (:file "raylib-manager")
                 (:file "raylib-bindings")
		 (:file "ui")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "common-gui/tests"))))

(defsystem "common-gui/tests"
  :author ""
  :license ""
  :depends-on ("common-gui"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for common-gui"
  :perform (test-op (op c) (symbol-call :rove :run c)))
