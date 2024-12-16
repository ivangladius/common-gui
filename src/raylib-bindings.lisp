
(uiop:define-package :raylib-bindings
  (:use :cl :cffi :raylib-manager))


(in-package #:raylib-bindings)


(ql:quickload :cffi-libffi)

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
;; (defparameter *callback-container-prio* (safe-ds::make-queue))

;; (declaim (optimize (debug 3) (safety 3) (speed 0)))
;; (defun execute-callback (callback-queue)
;;   "since we have container in container, if we click on the middle one
;;    how do we know which one is clicked, since they overlap and share the same area
;;    so we push fron a container which is a potential candidate, and then we take the front
;;    thus we will have the most deep nested one"
;;   (let* ((job (safe-ds::queue-front callback-queue)))
;;     (when job
;;       (let ((fun (car job))
;;             (args (cadr job)))
;;         ;; (print "executing...")
;;         (funcall fun args))
;;       ;; (format t "clearing ~a~%" (safe-ds::safe-queue-queue callback-queue))
;;       (safe-ds::queue-clear callback-queue))))


