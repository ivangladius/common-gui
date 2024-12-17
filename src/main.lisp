
(uiop:define-package common-gui
  (:use :cl :iv-utils
	:bordeaux-threads
	:raylib-bindings
	:local-time
   :ui
   :user))

(in-package #:common-gui)

(defparameter *gui-tree*
  (ui::container
   :name "base"
   :width 800 :height 600
   :color #xff399231
   :elements
   (list
    (ui::edit
     :container-name "login-edit-container"
     :text-name "login-edit-text"
     :width 200 :height 200))))

  
(defun main ()
  (ui::with-ui-loop ((800 600 "Hello" *gui-tree*) ui)
    (logic ui)))

(defun logic (gui)
  (wild-background gui :name "login-edit-container")
  )

(defparameter *main-id* nil)

(defun start ()
  (setf *main-id* (bt:make-thread #'(lambda () (main)))))

(defun end ()
  (unless (null *main-id*)
    (bt:destroy-thread *main-id*)
    (setf *main-id* nil)))

(defun restart-main ()
  (end)
  (start))
