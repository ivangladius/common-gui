
(defun build-raylib ()
  (let ((raylib-url "https://github.com/raysan5/raylib")
        (make-command "make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED -j$(nproc)"))
    (unless (probe-file #P"raylib")
      (uiop:run-program (format nil "git clone ~a" raylib-url) :output t))
    (uiop:run-program (format nil "bash -c 'cd raylib/src && ~a'" make-command) :output t)))
