
(uiop:define-package :safe-accessor
  (:use :cl :bordeaux-threads))

(in-package #:safe-accessor)

(defstruct safe-accessor
  obj
  mutex)


(defun create-safe-accessor (obj)
  (make-safe-accessor
   :obj obj
   :mutex (bt:make-lock)))

(defmacro with-lock (accessor &body body)
  `(let ((lock (safe-accessor-mutex ,accessor)))
       (bt:with-lock-held (lock)
	 (unwind-protect
	     (progn ,@body)))))
