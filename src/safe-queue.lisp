
(uiop:define-package :safe-ds
  (:use :cl :bordeaux-threads))

(in-package #:safe-ds)

(defstruct safe-queue
  queue
  mutex)

(defun make-queue ()
  (make-safe-queue :queue '()
                   :mutex (bt:make-lock)))

(defun queue-pop (queue)
  (let ((elem nil)
        (mutex (safe-queue-mutex queue)))
    (bt:with-lock-held (mutex)
      (setf elem (pop (safe-queue-queue queue)))
      elem)))

(defun queue-push (queue elem)
  (let ((mutex (safe-queue-mutex queue)))
    (bt:with-lock-held (mutex)
      (push elem (safe-queue-queue queue)))))

(defun queue-push-new (queue elem)
  (let ((mutex (safe-queue-mutex queue)))
    (bt:with-lock-held (mutex)
      (pushnew elem (safe-queue-queue queue)))))

(defun queue-clear (queue)
  (let ((mutex (safe-queue-mutex queue)))
    (bt:with-lock-held (mutex)
      (dolist (elem (safe-queue-queue queue))
        (pop (safe-queue-queue queue))))))

(defun queue-front (queue)
  (car (safe-queue-queue queue)))

(defun queue-back (queue)
  (let* ((q (safe-queue-queue queue))
         (len (length q)))
    (if (< 0 len)
        (nth (- len 1) q))))
