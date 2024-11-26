



;;;; Demo

;; CL-USER> (iv-find :path "dots" :iname "*version*")

;; ("dots/archive.project.backup.with.version.v2024.latest.yaml"
;;  "dots/data.analysis.sample.results.final.updated.version.csv"
;;  "dots/project.build.release.archive.version.1.0.final.zip"
;;  "dots/backup.file.system.image.2024.final.version.tar.gz"
;;  "dots/summary.of.the.final.report.updated.version.latest.json"
;;  "dots/record.session.date.time.file.version.v2.final.mp3")


   ;;;; Demo bulk rename,
;; (iv-rename-text-of-filenames-in-bulk "."
;; 				     :old "version"
;; 				     :new "xxxxxxx")

   ;;;; expands to 
;; (let ((#:g376 (iv-find :path "." :depth 5)))
;;   (dolist (f #:g376) (iv-rename-text-of-filename f "version" "xxxxxxx")))

   ;;;; after bulk rename
;; CL-USER> (iv-find :path "dots" :iname "*version*")

;; NIL

;; CL-USER> (iv-find :path "dots" :iname "*xxx*")

;; ("dots/analysis.data.final.report.updated.release.XXX.xml"
;;  "dots/test.results.for.the.project.final.xxx.complete.log"
;;  "dots/archive.project.backup.with.xxxxxxx.v2024.latest.yaml"
;;  "dots/data.analysis.sample.results.final.updated.xxxxxxx.csv"
;;  "dots/project.build.release.archive.xxxxxxx.1.0.final.zip"
;;  "dots/backup.file.system.image.2024.final.xxxxxxx.tar.gz"
;;  "dots/summary.of.the.final.report.updated.xxxxxxx.latest.json"
;;  "dots/record.session.date.time.file.xxxxxxx.v2.final.mp3")

(uiop:define-package :iv-utils
  (:use :cl :str)
  (:export #:iv-find))

(in-package #:iv-utils)

(defun iv-find (&key
                  (path (uiop:getcwd))
                  (type "f")
                  (iname "" iname-supplied-p)
                  (name "" name-supplied-p)
                  (depth 3)
                  (cl-path nil))
  (let ((find-str (format nil "find ~a ~a ~a -maxdepth ~a"
                          path
                          (format nil "-type ~a" type)
                          (cond
                            (iname-supplied-p (format nil "-iname \"~a\"" iname))
                            (name-supplied-p (format nil "-name \"~a\"" name))
                            (t ""))
                          depth)))
    (let ((result (str:lines (uiop:run-program find-str :output :string))))
      (if cl-path
          (mapcar #'pathname result)
          result))))

(defun iv-rename-text-of-filename (filename old new)
  (let* ((f (str:replace-all old new filename))
         (new-filename (merge-pathnames f (truename "."))))
    (rename-file filename new-filename)))


(defmacro iv-rename-text-of-filenames-in-bulk (dir
                                               &key
                                                 (filter-name "" filter-name-supplied-p)
                                                 (old nil)
                                                 (new nil)
                                                 (ignore-case t)
                                                 (depth 5))
  (let ((files (gensym)))
    `(let ((,files (iv-find :path ,dir
                            ,@(when filter-name-supplied-p
                                (if ignore-case
                                    `(:iname ,filter-name)
                                    `(:name ,filter-name)))
                            :depth ,depth)))
       (dolist (f ,files)
         (iv-rename-text-of-filename f ,old ,new)))))
