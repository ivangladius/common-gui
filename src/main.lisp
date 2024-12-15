
(uiop:define-package common-gui
  (:use :cl :iv-utils))

(in-package #:common-gui)



;; (defun main()
;;   (let ((screen-width 800)
;;         (screen-height 600))
;;     (with-window (screen-width screen-height "hello from cl")
;;       (set-target-fps 60)
;;       (loop
;;         :until (window-should-close)
;;         do (with-drawing
;;                (clear-background :raywhite)
;;              (draw-fps 20 20)
;;              (draw-text "hello world my dear!"))))))
