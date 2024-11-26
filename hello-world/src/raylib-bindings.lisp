
(uiop:define-package :raylib-bindings
  (:use :cl :cffi :raylib-manager :iv-utils :bordeaux-threads :safe-ds))

(in-package #:raylib-bindings)

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

(defun container (x y width height color
                  &key (callback nil) (callback-args nil)
                    (scale 1.0) (elements nil) (relx nil) (rely nil))
  (list :type :container :x x :y y :width width :height height :color color
        :scale scale :elements (if (listp elements) elements (list elements))
        :relx relx :rely rely
        :callback callback
        :callback-args callback-args))

(defun text (value relx rely font-size &optional (color #xffffffff))
  (list :type :text :value value :relx relx :rely rely :font-size font-size :color color))

(defun draw-element (element &key (parent-container nil) (callback-queue nil) (clicked nil) (scale 1))
  (let ((container-x (if parent-container (getf parent-container :x) 0))
        (container-y (if parent-container (getf parent-container :y) 0)))
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
         (let ((relx (or (getf element :relx) 0))
               (rely (or (getf element :rely) 0)))
           (draw-rectangle (+ (getf element :x) relx container-x)
                           (+ (getf element :y) rely container-y)
                           (getf element :width)
                           (getf element :height)
                           (getf element :color))
           (dolist (e (getf element :elements))
             (draw-element e :parent-container element
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

(defparameter *gui-elements*
  (container
   50 50 800 600 #xff141403
   :scale 1.0
   :callback #'(lambda (container)
                 (setf (getf container :color) #xff0000ff))
   ;; (format t  "you clicked me with args: ~a~%" container))
   :callback-args :self
   :elements
   (list
    (text "username" 80 30 30 #xffa0fff0)
    (text "password" 80 80 30 #xffa0fff0)
    (container
     0 0 50 50 #xfffa0000
     :relx 100 :rely 100
     :callback #'(lambda (container)
                   (setf (getf container :color) #xff00ff00))
     :callback-args :parent))))


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
  (with-window (1600 1200 "hello")
    (logic)))
