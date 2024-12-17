
(uiop:define-package user
  (:use :cl :iv-utils
	:bordeaux-threads
	:raylib-bindings
	:local-time
	:ui))

(in-package #:common-gui)


(defun timer-second (ui &key name)
  (let* ((sec (local-time:timestamp-second (local-time:now)))
	 (fmt (format nil "~a" sec)))
    (ui::change-element
     (ui::ui-safe-name-table ui)
     :name name
     :attributes
     (list
      :value fmt
      :font-size 50
      :color (if (= (mod sec 2) 0)
		 #xff00ff00
		 #xffffff00)
      ))))

(defun wild-background (ui &key name)
  (let* ((table (ui::ui-safe-name-table ui))
	 (obj (ui::get-element table :name name))
	(curr-color (getf obj :color)))
    (ui::change-element
     table
     :name name
     :attributes
     (list
      :color (+ curr-color 1)))
    ;; (ui::change-element
    ;;  gui
    ;;  :name "color-value"
    ;;  :attribute :value
    ;;  :value (format nil "~a" curr-color))
    ))

;; (defun colliding-moving-cube (ui field &key name)
;;   (unless name
;;     (error "time-second: name cannot be nil"))
;;   (let* ((table (ui::ui-safe-name-table ui))
;; 	 (obj (ui::get-element table :name name))
;; 	 (cont (getf obj :container))
;; 	 (direction-factor (getf obj :direction-factor))

;; 	 (posx (or (getf cont :relx) 0))
;; 	 (posy (or (getf cont :rely) 0))
;; 	 (width (or (getf cont :width) 0))
;; 	 (height (or (getf cont :height) 0)))
;;     (cond
;;       ((< (ui::ui-width ui) (+ posx width)) (setf (getf cont :direction-factor)
;; 						  (* direction-factor -1)))
;;       ((<= posx 0) (setf (getf cont :direction-factor)
;; 						  (* direction-factor -1)))
;;       ((<= (ui::ui-height ui) (+ posy height)) (setf (getf cont :direction-factor)
;; 						  (* direction-factor -1)))
;;       ((<= posy 0) (setf *factory* (setf (getf cont :direction-factor)
;; 					 (* direction-factor -1)))))))
    
    ;; (cond
    ;;   ((< (ui::ui-width ui) (+ posx width)) (setf *factorx* (* *factorx* -1)))
    ;;   ((<= posx 0) (setf *factorx* (* *factorx* -1)))
    ;;   ((<= (ui::ui-height ui) (+ posy height)) (setf *factory* (* *factory* -1)))
    ;;   ((<= posy 0) (setf *factory* (* *factory* -1))))
    ;; (format t "[~a ~a ~a ~a]~%" posx posy *factorx* *factory*)
    ;; (ui::change-element
    ;;  table
    ;;  :name "moving"
    ;;  :attributes
    ;;  (list
    ;;   :relx (+ posx *factorx*)
    ;;   :rely (+ posy *factory*)
    ;;   ))))


(defparameter *factorx* 1)
(defparameter *factory* 10)

(defun moving-cube (ui &key name)
  (unless name
    (error "time-second: name cannot be nil"))
  (let* ((table (ui::ui-safe-name-table ui))
	 (obj (ui::get-element table :name name))
	 (posx (or (getf obj :relx) 0))
	 (posy (or (getf obj :rely) 0))
	 (width (or (getf obj :width) 0))
	 (height (or (getf obj :height) 0)))
    (cond
      ((< (ui::ui-width ui) (+ posx width)) (setf *factorx* (* *factorx* -1)))
      ((<= posx 0) (setf *factorx* (* *factorx* -1)))
      ((<= (ui::ui-height ui) (+ posy height)) (setf *factory* (* *factory* -1)))
      ((<= posy 0) (setf *factory* (* *factory* -1))))
    (format t "[~a ~a ~a ~a]~%" posx posy *factorx* *factory*)
    (ui::change-element
     table
     :name name
     :attributes
     (list
      :relx (+ posx *factorx*)
      :rely (+ posy *factory*)
      ))))
