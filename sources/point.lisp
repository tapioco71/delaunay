;;;;-*- mode: common-lisp-mode; coding: utf-8; -*-
;;;; point.lisp
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

;; Point class.
(defclass point-class ()
  ((name
    :initarg :name
    :accessor name
    :initform nil)
   (coordinates
    :initarg :coordinates
    :accessor coordinates
    :initform nil)))

;; Functions.
(defun make-point (&rest parameters &key
				      (name (symbol-name (gensym "p-")) name-p)
				      (coordinates nil coordinates-p))
  (declare (ignorable parameters name coordinates))
  (when name-p
    (check-type name (or symbol keyword string (unsigned-byte 64) null)))
  (when coordinates-p
    (check-type coordinates (or grid:foreign-array null))
    (assert (= (length (grid:dimensions coordinates)) 1)))
  (make-instance 'point-class
		 :name name
		 :coordinates coordinates))

(defun point= (a b)
  (check-type a point-class)
  (check-type b point-class)
  (let ((temp-a (grid:copy-to (coordinates a) 'array))
	(temp-b (grid:copy-to (coordinates b) 'array)))
    (when (and (equalp (array-dimensions temp-a)
		       (array-dimensions temp-b))
	       (equalp (array-element-type temp-a)
		       (array-element-type temp-b)))
      (loop
	named main-loop
	for i from 0 below (array-total-size temp-a)
	finally (return-from main-loop t)
	when (/= (row-major-aref temp-a i)
		 (row-major-aref temp-b i))
	  do
	     (return-from main-loop nil)))))

(defun distance (a b)
  (when (equalp (grid:dimensions (coordinates a))
		(grid:dimensions (coordinates a)))
    (grid:norm (gsll:elt- (grid:copy (coordinates a)) (grid:copy (coordinates b))))))

;; Methods.
(defmethod print-object ((object point-class) output)
  (print-unreadable-object (object output :type t)
    (format output
	    "(:name ~s :coordinates ~s)"
	    (name object)
	    (coordinates object))))

;;;; End of file point.lisp
