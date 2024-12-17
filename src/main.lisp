
(uiop:define-package common-gui
  (:use :cl :iv-utils
	:bordeaux-threads
	:raylib-bindings
	:local-time
	:ui))

(in-package #:common-gui)


;;(change-element *gui* :name "title" :attribute :value :value "hello")

;; (defun timer-text (gui)
;;     (let* ((sec (local-time:timestamp-second (local-time:now)))
;; 	   (fmt (format nil "~a" sec)))
;;       (ui::change-element gui :name "title" :attribute :value :value fmt)
;;       (ui::change-element gui :name "title" :attribute :font-size :value 80)
;;       (ui::change-element gui :name "title" :attribute :color
;; 			      :value (if (= (mod sec 5) 0)
;; 					 #xff00ff00
;; 					 #xffffff00))
;;       (ui::change-element gui :name "title" :attribute :relx :value 200)
;;       (ui::change-element gui :name "title" :attribute :rely :value 200)))

(defun timer-second (ui &key name)
  (unless name
    (error "time-second: name cannot be nil"))
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
  (unless name
    (error "time-second: name cannot be nil"))
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

;; (defparameter *gui-tree*
;;   (ui::container
;;    :name "base"
;;    :width 800 :height 600
;;    :color #xff399231
;;    :elements
;;    (list
;;     (ui::moving-container
;;      :name "cube1"
;;      :width 100 :height 100 
;;      :relx 280 :rely 150
;;      :color #xff040302
;;      :elements
;;      (list
;;       (ui::text :name "cube1-text" :value "" :color #xffff0ff0
;; 		:relx 25 
;; 		:rely 25 
;; 		:font-size 30)))
;;     (ui::moving-container
;;      :name "cube2"
;;      :width 100 :height 100 
;;      :relx 100 :rely 20
;;      :color #xff04f302
;;      :elements
;;      (list
;;       (ui::text :name "cube2-text" :value "" :color #xff00ff00
;; 		:relx 25 
;; 		:rely 25 
;; 		:font-size 30)))

;;     (ui::moving-container
;;      :name "cube3"
;;      :width 100 :height 100 
;;      :relx 480 :rely 150
;;      :color #xfff40000
;;      :elements
;;      (list
;;       (ui::text :name "cube3-text" :value "" :color #xff00ff00
;; 		:relx 25 
;; 		:rely 25 
;; 		:font-size 30))))))

(defparameter *gui-tree*
  (ui::container
   :name "base"
   :width 800 :height 600
   :color #xff399231
   :elements
   (list
    (ui::container
	 :name "cube1"
	 :width 100 :height 100 
	 :relx 180 :rely 250
	 :color #xff040302)
    :elements
    (list
      (ui::text :name "cube1-text" :value "asdf" :color #xffff0ff0
		:relx 25 
		:rely 25 
		:font-size 30)))))

  
(defun main ()
  (ui::with-ui-loop ((800 600 "Hello" *gui-tree*) ui)
    (logic ui)))

(defun logic (gui)
  (wild-background gui :name "base")
  (wild-background gui :name "cube1")
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
