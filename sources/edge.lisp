;;;;-*- mode: common-lisp-mode; coding: utf-8; -*-
;;;; edge.lisp
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

;; Edge class.
(defclass edge-class ()
  ((name
    :initarg :name
    :accessor name
    :initform nil)
   (extrema
    :initarg :extrema
    :accessor extrema
    :initform nil)))

;; Functions.
(defun make-edge (&rest parameters &key
				     (name (symbol-name (gensym "e-")) name-p)
				     (extrema nil extrema-p))
  "Create an edge object using extrema point list."
  (declare (ignorable parameters name extrema))
  (when name-p
    (check-type name (or symbol keyword string (unsigned-byte 64) null)))
  (when extrema-p
    (check-type extrema (or list null))
    (assert (= (length extrema) 2))
    (loop
      for extremum in extrema
      do
	 (assert (typep extremum 'point-class))))
  (make-instance 'edge-class
		 :name name
		 :extrema extrema))

(defun make-edge-from-point-point (&rest parameters &key
						      (name (symbol-name (gensym "e-")) name-p)
						      (first-point nil first-point-p)
						      (second-point nil second-point-p))
  "Create and edge object using to given points."
  (declare (ignorable parameters name first-point second-point))
  (when name-p
    (check-type name (or symbol keyword string (unsigned-byte 64) null)))
  (when first-point-p
    (check-type first-point (or point-class null)))
  (when second-point-p
    (check-type second-point (or point-class null)))
  (make-edge :name name
	     :extrema (list first-point second-point)))

(defun edge= (a b)
  "Check if point a is equal to point b. The point names are not checked."
  (check-type a edge-class)
  (check-type b edge-class)
  (or (and (point= (nth 0 (extrema a)) (nth 0 (extrema b)))
	   (point= (nth 1 (extrema a)) (nth 1 (extrema b))))
      (and (point= (nth 0 (extrema a)) (nth 1 (extrema b)))
	   (point= (nth 1 (extrema a)) (nth 0 (extrema b))))))

;; Methods.
(defmethod print-object ((object edge-class) output)
  "Print the point-class object."
  (print-unreadable-object (object output :type t)
    (format output
	    "(:name ~s :extrema ~s)"
	    (name object)
	    (extrema object))))

(defmethod edge-length ((object edge-class))
  "Compute the edge length using two norm."
  (grid:norm (reduce #'(lambda (x y)
			 (gsll:elt- (grid:copy (coordinates x))
				    (grid:copy (coordinates y))))
		     (extrema object))))

;;;; end of edge.lisp
