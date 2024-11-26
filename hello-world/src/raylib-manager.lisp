
(uiop:define-package :raylib-manager
  (:use :cl :cffi :iv-utils))

(in-package #:raylib-manager)

(defparameter *raylib-loaded* nil)
(defparameter *project-root* (asdf:system-source-directory :hello-world))
(defparameter *project-raylib-dir* (merge-pathnames "raylib" *project-root*))

(defun raylib-build ()
  (let ((raylib-url "https://github.com/raysan5/raylib")
        (make-command "make PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED -j$(nproc)"))
    (unless (probe-file *project-raylib-dir*)
      (print "cloning raylib...")
      (uiop:run-program (format nil "git clone ~a ~a" raylib-url *project-raylib-dir*) :output t))
    (print "building raylib...")
    (uiop:run-program 
     (format nil "bash -c 'cd ~a && ~a'" *project-raylib-dir* make-command) :output t)))


(defun raylib-path ()
  (let ((raylib-path (first (iv-find :path *project-raylib-dir* :iname "*libraylib*"))))
    raylib-path))

(defun raylib-load ()
  (unless (probe-file *project-raylib-dir*)
    (error "cannot find raylib shared library, run (raylib-build)"))
  (let ((path (raylib-path)))
    (unless path
      (raylib-build))
    (cffi:load-foreign-library path)
    (setf *raylib-loaded* t)))

(defun raylib-remove ()
  (uiop:run-program "rm -rf raylib"))

(defun raylib-loaded? ()
  *raylib-loaded*)

(defun raylib-ensure-loaded ()
  (unless (raylib-loaded?)
    (raylib-load)))






