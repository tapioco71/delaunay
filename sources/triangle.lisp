;;;;-*- mode: common-lisp-mode; coding: utf-8; -*-
;;;; triangle.lisp
;;;;
;;;; Copyright 2024 Angelo Rossi <angelo.rossi.homelab@gmail.com>
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     1. Redistributions of source code must retain the above copyright notice,
;;;;        this list of conditions and the following disclaimer.
;;;;
;;;;     2. Redistributions in binary form must reproduce the above copyright
;;;;        notice, this list of conditions and the following disclaimer in the
;;;;        documentation and/or other materials provided with the distribution.
;;;;
;;;;     3. Neither the name of the copyright holder nor the names of its
;;;;        contributors may be used to endorse or promote products derived from
;;;;        this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;;;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:delaunay)

;; Triangle class.
(defclass triangle-class ()
  ((name
    :initarg :name
    :accessor name
    :initform nil)
   (vertices
    :initarg :vertices
    :accessor vertices
    :initform nil)))

;; Functions.
(defun make-triangle (&rest parameters &key
					 (name (symbol-name (gensym "tr-")) name-p)
					 (vertices nil vertices-p))
  "Create a triangle from a vertices list."
  (declare (ignorable parameters name vertices))
  (when name-p
    (check-type name (or symbol keyword string (unsigned-byte 64) null)))
  (when vertices-p
    (check-type vertices (or list null))
    (assert (= (length vertices) 3))
    (loop
      for vertex in vertices
      do
	 (assert (equalp (type-of vertex) 'point-class))))
  (make-instance 'triangle-class
		 :name name
		 :vertices vertices))

(defun make-triangle-from-points (&rest parameters &key
						     (name (symbol-name (gensym "tr-")) name-p)
						     (first-point nil first-point-p)
						     (second-point nil second-point-p)
						     (third-point nil third-point-p))
  "Create a triangle using 3 points (vertices)."
  (declare (ignorable parameters name first-point second-point third-point))
  (when name-p
    (check-type name (or symbol keyword string (unsigned-byte 64) null)))
  (when first-point-p
    (check-type first-point (or point-class null)))
  (when second-point-p
    (check-type second-point (or point-class null)))
  (when third-point-p
    (check-type third-point (or point-class null)))
  (make-triangle :name name
		 :vertices (list first-point second-point third-point)))

(defun make-triangle-from-edge-point (&rest parameters &key
							 (name (symbol-name (gensym "tr-")) name-p)
							 (edge nil edge-p)
							 (point nil point-p))
  "Create a triangle from an edge (2 points) and a point (1 point)."
  (declare (ignorable parameters name edge point))
  (when name-p
    (check-type name (or symbol keyword string (unsigned-byte 64) null)))
  (when edge-p
    (check-type edge (or edge-class null)))
  (when point-p
    (check-type point (or point-class null)))
  (make-triangle :name name
		 :vertices (append (extrema edge) (list point))))

(defun triangle= (a b)
  (check-type a triangle-class)
  (check-type b triangle-class)
  (loop
    named main-loop
    for vertex in (vertices a)
    finally (return-from main-loop t)
    unless (find-if #'(lambda (point)
			(point= point vertex))
		    (vertices b))
      do
	 (return-from main-loop nil)))

;; Methods.
(defmethod print-object ((object triangle-class) output)
  "Pretty print triangle object."
  (print-unreadable-object (object output :type t)
    (format output
	    "(:name ~s :vertices ~s)"
	    (name object)
	    (vertices object))))

(defmethod compute-circumcentre ((object triangle-class))
  "Compute the circle for three points (circumcircle)."
  (let ((center nil)
	(delta nil)
	(x (grid:make-foreign-array 'double-float :dimensions '(3) :initial-element 0d0))
	(y (grid:make-foreign-array 'double-float :dimensions '(3) :initial-element 0d0))
	(norm (grid:make-foreign-array 'double-float :dimensions '(3) :initial-element 0d0))
	(ok? nil))
    ;;
    (loop
      for i from 0 below 3
      do
	 (setf (grid:gref x i) (grid:gref (coordinates (nth i (vertices object))) 0))
	 (setf (grid:gref y i) (grid:gref (coordinates (nth i (vertices object))) 1))
	 (setf (grid:gref norm i) (+ (expt (grid:gref x i) 2d0) (expt (grid:gref y i) 2d0))))
    (setq delta (- (* (- (grid:gref x 1) (grid:gref x 0)) (- (grid:gref y 2) (grid:gref y 0)))
		   (* (- (grid:gref x 2) (grid:gref x 0)) (- (grid:gref y 1) (grid:gref y 0)))))
    (setq center (grid:make-foreign-array 'double-float
					  :initial-contents (list (/ (- (* (- (grid:gref norm 1) (grid:gref norm 0)) (- (grid:gref y 2) (grid:gref y 0)))
									(* (- (grid:gref norm 2) (grid:gref norm 0)) (- (grid:gref y 1) (grid:gref y 0))))
								     (* 2d0 delta))
								  (/ (- (* (- (grid:gref x 1) (grid:gref x 0)) (- (grid:gref norm 2) (grid:gref norm 0)))
									(* (- (grid:gref x 2) (grid:gref x 0)) (- (grid:gref norm 1) (grid:gref norm 0))))
								     (* 2d0 delta))
								  0d0)))
    (setq ok? t)
    (values (make-point :coordinates center)
	    (grid:norm (gsl:elt- (grid:copy (coordinates (nth 0 (vertices object))))
				 (grid:copy center)))
	    ok?)))

(defmethod inside-circumcircle? ((object triangle-class) point)
  "Check if point lies inside circumcircle for the triangle."
  (multiple-value-bind (center radius ok?)
      (compute-circumcentre object)
    (when ok?
      (<= (grid:norm (gsll:elt- (grid:copy (coordinates center))
				(grid:copy (coordinates point))))
	  radius))))

(defmethod get-edges ((object triangle-class))
  "Return the three triangle edges computed from its vertices."
  (loop
    for i from 0 below 3
    collect (make-edge :extrema (list (nth i (vertices object))
				      (nth (mod (1+ i) 3) (vertices object))))))

;;;; End of file edge.lisp
