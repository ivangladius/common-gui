
(uiop:define-package :ui
  (:use :cl
   :iv-utils
	:bordeaux-threads
   :safe-ds
	:safe-accessor
   :raylib-bindings))

(in-package #:ui)

(defstruct ui
  gui
  safe-name-table
  safe-event-queue)

(defun ui-create (gui-tree)
  (let ((ui (make-ui
	     :gui gui-tree
	     :safe-name-table
	     (safe-accessor::create-safe-accessor
	      (make-hash-table :test 'equal))
	     :safe-event-queue (safe-ds::make-queue))))
    (ui-initialize (ui-safe-name-table ui) gui-tree)
    ui))


(defun ui-initialize (safe-table elements)
  (let ((table (safe-accessor::safe-accessor-obj safe-table)))
    (setf (gethash (getf elements :name) table) elements)
    (dolist (elem (getf elements :elements))
      (setf (gethash (getf elem :name) table) elem)
      (if (eq (getf elem :type) :container)
	  (ui-initialize safe-table elem)))
    table))

(defun get-mouse-position ()
  (let ((vector2-object (raylib-bindings::%get-mouse-position)))
    (values (getf vector2-object 'raylib-bindings::x) (getf vector2-object 'raylib-bindings::y))))


(defmacro with-window ((width height title) &body body)
  `(progn
     (raylib-bindings::init-window ,width ,height ,title)
     (unwind-protect
          (loop
            :until (raylib-bindings::window-should-close)
            :do (progn
                  ,@body))
       (raylib-bindings::close-window))))

(defmacro with-drawing (&body body)
  `(progn
     (raylib-bindings::begin-drawing)
     (progn ,@body)
     (raylib-bindings::end-drawing)))


(declaim (optimize (debug 3)))
(defun %clicked-container? (mousex mousey container)
  (format t "[~a, ~a]~%" mousex mousey)
  (when (and mousex mousey)
    (let ((relx (or (getf container :relx) 0))
	  (rely (or (getf container :rely) 0)))
      (let ((x (+ (getf container :x) relx))
	    (y (+ (getf container :y) rely))
	    (width (getf container :width))
	    (height (getf container :height)))
	(and (<= (- x 10 ) mousex) (<= mousex (+ x width 10))
	     (<= (- y 10) mousey) (<= mousey (+ y height 10)))))))


(declaim (optimize (debug 3)))
(defun clicked-container? (container)
  (multiple-value-bind (x y) (get-mouse-position)
    (%clicked-container? x y container)))

(defun container (&key (name nil) (x 0) (y 0) (width 0) (height 0) (color #xffffffff)
                  (callback nil) (callback-args nil)
                    (scale 1.0) (elements nil) (relx 0) (rely 0))
  (when (or (= width 0) (= height 0))
    (error "container cannot have empty or 0 width and height"))
  (list :type :container :name name :x x :y y :width width :height height :color color
        :scale scale :elements (if (listp elements) elements (list elements))
        :relx relx :rely rely
        :callback callback
        :callback-args callback-args))

(defun text (value &key (name nil) (relx 0) (rely 0) (font-size 15) (color #xffffffff))
  (list :type :text :name name :value value :relx relx :rely rely :font-size font-size :color color))

(defun draw-elements (element &key (parent-relx nil) (parent-rely nil) (parent-container nil) (callback-queue nil) (clicked nil) (scale 1))
  (let ((container-x (or parent-relx (getf element :x)))
        (container-y (or parent-rely (getf element :y))))
    (case (getf element :type)
      (:text
       (raylib-bindings::draw-text (getf element :value)
                  (+ (getf element :relx) container-x)
                  (+ (getf element :rely) container-y)
                  (round (* (getf element :font-size) scale))
                  (getf element :color)))
      (:container
       (progn
         (when (and clicked (clicked-container? element))
	   (safe-ds::queue-push-new callback-queue (getf element :name)) 
	   (format t "pushed: ~a~%" (safe-ds::safe-queue-queue callback-queue))))
         ;; (funcall callback element)
         ;; (funcall callback callback-args)))) TODO: fix this either relx or x not both
	 (let ((newx (+ (getf element :relx) container-x))
	       (newy (+ (getf element :rely) container-y)))
           (raylib-bindings::draw-rectangle-gradientv newx
                           newy
                           (getf element :width)
                           (getf element :height)
                           (getf element :color)
			   #xff090909)
           (dolist (e (getf element :elements))
	     (draw-elements e
			   :parent-relx newx
			   :parent-rely newy
			   :parent-container element
			   :clicked clicked
			   :callback-queue callback-queue
			   :scale (getf element :scale))))))))

(defun has-attribute (obj attribute)
  (getf obj attribute))

(defun get-element (safe-table &key (name nil))
  (unless name
    (error "get-element: name is nil"))
  (gethash name (safe-accessor::safe-accessor-obj safe-table)))

(defun change-element (safe-table &key (name nil) (attribute nil) (value nil))
  (unless (and name attribute value)
    (error "change-element: no attribute should be nil"))
  (safe-accessor::with-lock safe-table
    ;; (let* ((table (get-element safe-table))
    ;; 	   (obj (gethash name table)))
    (let ((obj (get-element safe-table :name name)))
	(setf (getf obj attribute) value))))


(declaim (optimize (debug 3)))
(defun execute-callback (ui-safe-name-table element-name)
  (when element-name
    (format t "executing: ~a~%" element-name))
  (when (and ui-safe-name-table element-name)
    (let* ((obj (get-element ui-safe-name-table :name element-name))
	   (fun (getf obj :callback))
	   (args (getf obj :callback-args)))
      (format t "fun: ~a~%" fun)
      (format t "args: ~a~%" args)
      (if fun
	  (if args
	      (funcall fun args)
	      (funcall fun))))))
    


