
(uiop:define-package common-gui
  (:use :cl :iv-utils
	:bordeaux-threads
	:raylib-bindings
	:ui))

(in-package #:common-gui)

(defparameter *gui-tree*
  (ui::container
   :name "bottom"
   :callback #'(lambda () (uiop:run-program "nautilus --new-window &"))
   :width 400 :height 400
   :elements
   (list
    (ui::container
     :name "middle"
     :callback #'(lambda () (uiop:run-program "chromium --new-window&"))
     :relx 100 :rely 100 :width 200 :height 400 :color #xff010101)
    (ui::container
     :name "top"
     :callback #'(lambda () (uiop:run-program "alacritty &"))
     :width 150 :height 300 :color #xff069694)
    ;;(ui::text "hello world" :name "title" :relx 0 :rely 0 :font-size 30 :color #xffA1A9ff)
    )))



;;(change-element *gui* :name "title" :attribute :value :value "hello")

(declaim (optimize (debug 3)))

(defmacro with-ui-loop (&body body)
  `(progn
     (raylib-bindings::set-target-fps 60)
     (let ((ui (ui::ui-create *gui-tree*)))
       (ui::with-drawing
	 (raylib-bindings::clear-background #x003300) ;; dark green
	 (let* ((clicked-p (raylib-bindings::left-mouse-clicked?))
		(name-table (ui::ui-safe-name-table ui))
		(event-queue (ui::ui-safe-event-queue ui)))
	   (ui::draw-elements (ui::ui-gui ui)
			      :clicked clicked-p
			      :callback-queue event-queue)
	   (progn
	     ,@body)
	   (let ((ev (safe-ds::queue-front event-queue)))
	     (when ev
	       (ui::execute-callback name-table ev)
	       (safe-ds::queue-clear event-queue))))))))

(defun logic ()
  (with-ui-loop
    (uiop:run-program "alacritty&")))
	
      ;; (if element-name
      ;; 	  (format t "executing: ~a~%" element-name)
      ;; 	  (format t "EMPTYYY" element-name))

      ;;(print (safe-ds::safe-queue-queue (ui::ui-safe-event-queue ui)))
      ;;(execute-callback *callback-container-prio*)
      ;; (let ((front (safe-ds::queue-front event-queue)))
      ;; 	(when front
      ;; 	  (format t "taking ~a~%" front)))
      ))))

(defun main()
  (ui::with-window (800 600 "hello")
    (logic)))



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
