
(uiop:define-package :raylib-bindings
  (:use :cl :cffi :raylib-manager :iv-utils :bordeaux-threads :safe-ds :safe-accessor))

(in-package #:raylib-bindings)

;;(raylib-manager::raylib-ensure-loaded)

(raylib-manager::raylib-ensure-loaded)


(defcstruct Vector2
  (x :float)
  (y :float))

;; Vector2 GetMousePosition(void);                         // Get mouse position XY
(defcfun ("GetMousePosition" %get-mouse-position) (:struct Vector2))

;; MOUSE_BUTTON_LEFT    = 0,       // Mouse button left
;; bool IsMouseButtonPressed(int button);                  // Check if a mouse button has been pressed once
(defconstant +mouse-button-left+ 0)

(defcfun ("IsMouseButtonPressed" is-mouse-button-pressed) :bool
  (button :int))

(defun left-mouse-clicked? ()
  (is-mouse-button-pressed +mouse-button-left+))

;; void InitWindow(int width, int height, const char *title);
(defcfun ("InitWindow" init-window) :void
  (width :int) (height :int) (title :string))

;; bool WindowShouldClose(void);                               // Check if application should c
(defcfun ("WindowShouldClose" window-should-close) :bool)

;; void BeginDrawing(void);                                    // Setup canvas (framebuffer) to
(defcfun ("BeginDrawing" begin-drawing) :void)

;; void EndDrawing(void);
(defcfun ("EndDrawing" end-drawing) :void)

;; void DrawRectangle(int posX, int posY, int width, int height, Color color);                        // Draw a color-filled rectangle
(defcfun ("DrawRectangle" draw-rectangle) :void
  (posX :int) (posY :int) (width :int) (height :int) (color :long))

;; void DrawRectangleGradientV(int posX, int posY, int width, int height, Color top, Color bottom);   // Draw a vertical-gradient-filled rectangle
(defcfun ("DrawRectangleGradientV" draw-rectangle-gradientv) :void
  (posX :int) (posY :int) (width :int) (height :int) (top :long) (bottom :long))

;; void DrawText(const char *text, int posX, int posY, int fontSize, Color color);       // Draw text (using default font)
(defcfun ("DrawText" draw-text) :void
  (text :string) (posX :int) (posY :int) (font-size :int) (color :long))

(defcfun ("ClearBackground" clear-background) :void
  (color :long))

;; void SetTargetFPS(int fps);                                 // Set target FPS (maximum)
(defcfun ("SetTargetFPS" set-target-fps) :void
  (fpt :int))

;; void CloseWindow(void);                                     // Close window and unload OpenG
(defcfun ("CloseWindow" close-window) :void)

(defun get-mouse-position ()
  (let ((vector2-object (%get-mouse-position)))
    (values (getf vector2-object 'x) (getf vector2-object 'y))))

(ql:quickload :cffi-libffi)

;; (values (foreign-slot-value vector2-object '(:struct vector2) 'x)
;;         (foreign-slot-value vector2-object '(:struct vector2) 'y))))

(defmacro with-window ((width height title) &body body)
  `(progn
     (init-window ,width ,height ,title)
     (unwind-protect
          (loop
            :until (window-should-close)
            :do (progn
                  ,@body))
       (close-window))))

(defmacro with-drawing (&body body)
  `(progn
     (begin-drawing)
     (progn ,@body)
     (end-drawing)))

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



;; (defun button-create (x y width height text font-size &optional color on-click)
;;   (let* ((%rect (make-rectangle :x x :y y
;;                                 :width width :height height :color color))
;;          (%text (make-text :content text
;;                            :x (+ x 110)
;;                            :y (+ y 50)
;;                            :font-size font-size :color #xffffffff))
;;          (%btn (make-button :rectangle %rect :text %text)))
;;     %btn))

;; (defparameter *buttons* '())

;; (defun draw-button (x y width height text font-size &key (color #xffffffff) (on-click nil))
;;   (let* ((btn (button-create x y width height text font-size color on-click))
;;          (rect (button-rectangle btn))
;;          (text (button-text btn)))
;;     (draw-rectangle (rectangle-x rect) (rectangle-y rect)
;;                     (rectangle-width rect) (rectangle-height rect) color)
;;     (draw-text (text-content text) (text-x text) (text-y text) (text-font-size text) (text-color text))
;;     (pushnew btn *buttons*)))


;; (setf *gui-elements* '())


;; TODO: ugly as fuck
(defun %clicked-container? (mousex mousey container)
  (format t "[~a, ~a]~%" mousex mousey)
  (let ((relx (or (getf container :relx) 0))
        (rely (or (getf container :rely) 0)))
    (let ((x (+ (getf container :x) relx))
          (y (+ (getf container :y) rely))
          (width (getf container :width))
          (height (getf container :height)))
      (and (<= (- x 10 ) mousex) (<= mousex (+ x width 10))
           (<= (- y 10) mousey) (<= mousey (+ y height 10))))))

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

(defun draw-element (element &key (parent-relx nil) (parent-rely nil) (parent-container nil) (callback-queue nil) (clicked nil) (scale 1))
  (let ((container-x (or parent-relx (getf element :x)))
        (container-y (or parent-rely (getf element :y))))
    (case (getf element :type)
      (:text
       (draw-text (getf element :value)
                  (+ (getf element :relx) container-x)
                  (+ (getf element :rely) container-y)
                  (round (* (getf element :font-size) scale))
                  (getf element :color)))
      (:container
       (progn
         (when (and clicked (clicked-container? element))
           (let ((callback (getf element :callback))
                 (callback-args (getf element :callback-args)))
             (if (eql callback-args :self)
                 (safe-ds::queue-push callback-queue (list callback element))
                 (if (and parent-container (eql :parent (getf element :callback-args)))
                     (safe-ds::queue-push callback-queue (list callback parent-container))
                     (safe-ds::queue-push callback-queue (list callback callback-args))))
             (format t "pushed: ~a~%" (safe-ds::safe-queue-queue callback-queue))))
         ;; (funcall callback element)
         ;; (funcall callback callback-args)))) TODO: fix this either relx or x not both
	 (let ((newx (+ (getf element :relx) container-x))
	       (newy (+ (getf element :rely) container-y)))
           (draw-rectangle-gradientv newx
                           newy
                           (getf element :width)
                           (getf element :height)
                           (getf element :color)
			   #xff090909)
           (dolist (e (getf element :elements))
	     (draw-element e
			   :parent-relx newx
			   :parent-rely newy
			   :parent-container element
			   :clicked clicked
			   :callback-queue callback-queue
			   :scale (getf element :scale)))))))))

(defparameter *callback-container-prio* (safe-ds::make-queue))

(declaim (optimize (debug 3) (safety 3) (speed 0)))
(defun execute-callback (callback-queue)
  "since we have container in container, if we click on the middle one
   how do we know which one is clicked, since they overlap and share the same area
   so we push fron a container which is a potential candidate, and then we take the front
   thus we will have the most deep nested one"
  (let* ((job (safe-ds::queue-front callback-queue)))
    (when job
      (let ((fun (car job))
            (args (cadr job)))
        ;; (print "executing...")
        (funcall fun args))
      ;; (format t "clearing ~a~%" (safe-ds::safe-queue-queue callback-queue))
      (safe-ds::queue-clear callback-queue))))

;; (defparameter *gui-elements*
;;   (container
;;    :x 0 :y 0 :width 800 :height 600 :color #xff141403
;;    :callback #'(lambda (container)
;;                  (setf (getf container :color) #xff0000ff))
;;    ;; (format t  "you clicked me with args: ~a~%" container))
;;    :callback-args :self
;;    :elements
;;    (list
;;     (container
;;      :color #xff443322
;;      :relx 300 :rely 200
;;      :width 200 :height 200
;;      :callback #'(lambda (container)
;;                    (setf (getf container :color) #xff00ff00))
;;      :callback-args :parent
;;      :elements
;;      (list
;; 	(text "username" :relx 30 :rely 20 :font-size 30 :color #xffa0fff0)
;; 	(text "password" :relx 30 :rely 80 :font-size 30 :color #xffa0fff0)
;;      )))))


(defparameter *gui* (make-hash-table :test 'equal))
(defparameter *gui-accessor* (create-safe-accessor *gui*))

(defparameter *gui-elements*
  (container
   :name "base"
   :x 200 :y 80
   :width 400 :height 400
   :elements
   (list
    (container :name "top" :width 200 :height 400 :color #xff010101)
    (container :name "bottom" :width 150 :height 300 :color #xff069694)
    (text "hello world" :name "title" :relx 0 :rely 0 :font-size 30 :color #xffA1A9ff)
    )))

(defun initialize-gui (gui-table root-element)
  (setf (gethash (getf root-element :name) gui-table) root-element)
  (dolist (elem (getf root-element :elements))
    (setf (gethash (getf elem :name) gui-table) elem)
    (if (eq (getf elem :type) :container)
	(initialize-gui gui-table elem)))
  gui-table)

(defun change-color (table name color)
  (with-lock-accessor *gui-accessor*
      (let ((obj (gethash name table)))
        (setf (getf obj :color) color))))

(change-color *gui* "bottom" #xff000022)

(initialize-gui *gui* *gui-elements*)


(defun logic ()
  (set-target-fps 60)
  (with-drawing
    (clear-background #x003300) ;; dark green
    (let ((clicked-p (left-mouse-clicked?)))
      (draw-element *gui-elements*
                    :clicked clicked-p
                    :callback-queue *callback-container-prio*))
    (execute-callback *callback-container-prio*)))

;;(draw-text "hello" 200 200 80 #xffffffff))))

(defun main()
  (with-window (800 600 "hello")
    (logic)))
