;;;;-*- mode: common-lisp-mode; coding: utf-8; -*-
;;;; mesh.lisp
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

;; Functions.
(defun make-super-triangle (points &rest parameters &key (verbose nil))
  "Create a supertriangle which engulf all the points in the fucking set."
  (declare (ignorable parameters verbose))
  (when points
    (multiple-value-bind (xc yc xmin ymin xmax ymax)
	(loop
	  for point in points
	  finally (return (values (/ xc (length points)) (/ yc (length points)) xmin ymin xmax ymax))
	  summing (grid:gref (coordinates point) 0) into xc
	  summing (grid:gref (coordinates point) 1) into yc
	  minimize (grid:gref (coordinates point) 0) into xmin
	  minimize (grid:gref (coordinates point) 1) into ymin
	  maximize (grid:gref (coordinates point) 0) into xmax
	  maximize (grid:gref (coordinates point) 1) into ymax)
      (let ((d (max (sqrt (+ (expt (- xmin xc) 2d0)
			     (expt (- ymin yc) 2d0)))
		    (sqrt (+ (expt (- xmax xc) 2d0)
			     (expt (- ymax yc) 2d0)))))
	    (phi (* (/ pi 5d0) (random 10))))
	(make-triangle :vertices (loop
				   for i from 0 below 3
				   collect (make-point :coordinates (grid:make-foreign-array 'double-float
											     :initial-contents (list (* 10d0 d (cos (+ phi (* i (/ (* 2d0 pi) 3d0)))))
														     (* 10d0 d (sin (+ phi (* i (/ (* 2d0 pi) 3d0)))))
														     0d0)))))))))

(defun unique-edges (edges)
  "Return unique edges in the set."
  (let ((return-edges nil))
    (loop
      with unique = nil
      for i from 0 below (length edges)
      do
	 (setq unique t)
	 (loop
	   named inner-loop
	   for j from 0 below (length edges)
	   when (/= i j)
	     do
		(when (edge= (nth i edges) (nth j edges))
		  (setq unique nil)
		  (return-from inner-loop)))
	 (when unique
	   (push (nth i edges) return-edges)))
    return-edges))

(defun shared-vertex? (tr1 tr2)
  "Return t if a shared vertex among two triangles exists otherwise nil."
  (check-type tr1 (or triangle-class null))
  (check-type tr2 (or triangle-class null))
  (let ((vertices1 (vertices tr1))
	(vertices2 (vertices tr2)))
    (loop
      named main-loop
      for p1 in vertices1
      finally (return-from main-loop nil)
      do
	 (loop
	   for p2 in vertices2
	   when (point= p1 p2)
	     do
		(return-from main-loop t)))))

(defun bowyer-watson-triangulation (points)
  "Bowyer-Watson Delaunay triangolation algotithm (see wikipedia page)."
  (let* ((super-triangle (make-super-triangle points))
	 (triangulation (list super-triangle)))
    (loop
      named main-loop
      with bad-triangles = nil
      with polygon = nil
      with bad-edges = nil
      for point in points
      do
	 (setq bad-triangles nil)
	 (loop
	   for triangle in triangulation
	   when (inside-circumcircle? triangle point)
	     do
		(push triangle bad-triangles))
	 (setq polygon nil)
	 (setq bad-edges nil)
	 (loop
	   with triangle-edges = nil
	   for triangle in bad-triangles
	   do
	      (setq triangle-edges (get-edges triangle))
	      (loop
		for triangle-edge in triangle-edges
		do
		   (push triangle-edge bad-edges)))
	 (setq polygon (unique-edges bad-edges))
	 ;;
	 (loop
	   for triangle in bad-triangles
	   do
	      (setq triangulation (remove-if #'(lambda (x)
						 (triangle= x triangle))
					     triangulation)))
	 (loop
	   for edge in polygon
	   do
	      (push (make-triangle-from-edge-point :edge edge :point point) triangulation)))
    (setq triangulation (remove-if #'(lambda (x)
				       (shared-vertex? x super-triangle))
				   triangulation))
    triangulation))

;;;; End of file mesh.lisp
