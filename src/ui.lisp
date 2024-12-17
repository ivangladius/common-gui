
(uiop:define-package :ui
  (:use :cl
   :iv-utils
	:bordeaux-threads
	:fmt
   :safe-ds
	:safe-accessor
   :raylib-bindings))

(in-package #:ui)

(defstruct ui
  gui
  safe-name-table
  safe-event-queue
  width
  height)

(defun ui-create (gui-tree width height)
  (unless (and width height)
    (error "ui-create: with and or height missing"))
  (let ((ui (make-ui
	     :gui gui-tree
	     :safe-name-table
	     (safe-accessor::create-safe-accessor
	      (make-hash-table :test 'equal))
	     :safe-event-queue (safe-ds::make-queue)
	     :width width
	     :height height)))
    (ui-initialize (ui-safe-name-table ui) gui-tree)
    ui))


(defun ui-initialize (safe-table elements)
  ;; get root element
  (let ((table (safe-accessor::safe-accessor-obj safe-table)))
    (setf (gethash (getf elements :name) table) elements)
    ;; iterate trough child elements recursively
    (dolist (elem (getf elements :elements))
      (setf (gethash (getf elem :name) table) elem)
      (case (getf elem :type)
	(:container (ui-initialize safe-table elem))
	(:edit (ui-initialize safe-table (getf elem :container)))))
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
	:focused? nil
        :callback callback
        :callback-args callback-args))

(defun text (&key (value "") (name nil) (relx 0) (rely 0) (font-size 20) (color #xffffffff))
  (list :type :text :name name :value value :relx relx :rely rely :font-size font-size :color color))

(defun edit (&key (hint "") (container-name nil) (text-name)
		  (width 0) (height 0)
	       (container-relx 0) (container-rely 0) (text-relx 0) (text-rely 0)
	       (font-size 20) (container-color #xffffffff) (text-color #xff000000))
  (unless (and width height)
    (error "edit: width or height cannot be 0"))
  (list
   :type :edit
   :container (container :name container-name :width width :height height :relx container-relx :rely container-rely :color container-color
			 :elements
			 (list
			  (text :name text-name :value hint :relx text-relx :rely text-rely :font-size font-size :color text-color)))))

(defun moving-container (&key (direction-factor 1) (name nil) (width nil) (height 0)
			   (relx 0) (rely 0) (color #xff000000) (elements nil))
  (unless (and name width height)
    (error "moving-container: no name provided"))
  (list :type :moving-container :container (container :name name :width width :height :height :relx relx :rely rely :color color :elements elements)
	:direction-factor direction-factor))


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
      (:moving-container
       (progn
	 (draw-elements (getf element :container)
			:parent-container element
			:clicked clicked
			:callback-queue callback-queue
			:scale (getf element :scale))))
      (:edit
       (progn
	 (draw-elements (getf element :container)
			:parent-container element
			:clicked clicked
			:callback-queue callback-queue
			:scale (getf element :scale))))
      (:container
       (progn
	 ;; TODO: need ui object to set focus? to t on click, IMPORTANT
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
  (let ((obj (gethash name (safe-accessor::safe-accessor-obj safe-table))))
      (unless obj
	(fmt::error-fmt ":name ~a not found in table~%" name)
	nil)
    obj))

(defun change-element (safe-table &key (name nil)
				    (attribute nil) (value nil) (attributes nil))
  (unless name
    (error "change-element: no attribute should be nil"))
  (safe-accessor::with-lock safe-table
    ;; (let* ((table (get-element safe-table))
    ;; 	   (obj (gethash name table)))
    (let ((obj (get-element safe-table :name name)))
      (if attributes
	  (loop :for attr = (pop attributes)
		:for val = (pop attributes)
		:while (and attr val)
		:do (setf (getf obj attr) val)))
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
    

(defmacro with-ui-loop (((width height title gui-tree) ui) &body body)
  `(progn
     (ui::with-window (,width ,height ,title)
       (raylib-bindings::set-target-fps 60)
       (let ((,ui (ui::ui-create ,gui-tree ,width ,height)))
	 (ui::with-drawing
	   (raylib-bindings::clear-background #x003300) ;; dark green
	   (let* ((clicked-p (raylib-bindings::left-mouse-clicked?))
		  (safe-table (ui::ui-safe-name-table ,ui))
		  (event-queue (ui::ui-safe-event-queue ,ui)))
	     (ui::draw-elements (ui::ui-gui ,ui)
				:clicked clicked-p
				:callback-queue event-queue)
	     (progn
	       ,@body)
	     (let ((ev (safe-ds::queue-front event-queue)))
	       (when ev
		 (ui::execute-callback safe-table ev)
		 (safe-ds::queue-clear event-queue)))))))))


