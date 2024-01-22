;;;; delaunay.lisp

(in-package #:delaunay)

;; Functions
(defun main (points)
  (check-type points (or list null))
  (bowyer-watson-triangulation points))
