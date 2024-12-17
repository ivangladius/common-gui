
(uiop:define-package fmt
  (:use :cl :iv-utils
	:bordeaux-threads
	:raylib-bindings
	:local-time
	:ui))

(in-package #:fmt)


(defun error-fmt (fmt-str args)
  (error (format nil fmt-str args)))
  


