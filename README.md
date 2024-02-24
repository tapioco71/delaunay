# delaunay
### _Angelo Rossi <angelo.rossi.homelab@gmail.com>_

Delaunay mesh generator using Bowyer-Watson algorithm (see https://en.wikipedia.org/wiki/Bowyer%E2%80%93Watson_algorithm)

To load and use the library in the slime environment:

CL-USER> (ql:quickload :delaunay)
To load "delaunay":
  Load 1 ASDF system:
    delaunay
; Loading "delaunay"
..................................................
[package delaunay]..
(:DELAUNAY)
CL-USER> (defparameter *points* nil)
*POINTS*
CL-USER> (loop
	   for i from 0 below 3
	   do
	      (loop
		for j from 0 below 3
		do
		   (push (delaunay::make-point :coordinates (grid:make-foreign-array 'double-float
										      :initial-contents (list (+ -1d0 (coerce i 'double-float))
													      (+ -1d0 (coerce j 'double-float))
													      0d0)))
			 *points*)))
NIL
CL-USER> *points*
(#<DELAUNAY::POINT-CLASS (:name "p-480" :coordinates #m(1.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-479" :coordinates #m(1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-478" :coordinates #m(1.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-477" :coordinates #m(0.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-476" :coordinates #m(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-475" :coordinates #m(0.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-474" :coordinates #m(-1.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-473" :coordinates #m(-1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
 #<DELAUNAY::POINT-CLASS (:name "p-472" :coordinates #m(-1.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>)
CL-USER> (delaunay::bowyer-watson-triangulation *points*)
(#<DELAUNAY::TRIANGLE-CLASS (:name "tr-683" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-476" :coordinates #m(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-473" :coordinates #m(-1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-472" :coordinates #m(-1.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>))>
 #<DELAUNAY::TRIANGLE-CLASS (:name "tr-682" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-475" :coordinates #m(0.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-476" :coordinates #m(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-472" :coordinates #m(-1.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>))>
 #<DELAUNAY::TRIANGLE-CLASS (:name "tr-648" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-477" :coordinates #m(0.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-474" :coordinates #m(-1.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-473" :coordinates #m(-1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>))>
 #<DELAUNAY::TRIANGLE-CLASS (:name "tr-647" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-476" :coordinates #m(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-477" :coordinates #m(0.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-473" :coordinates #m(-1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>))>
 #<DELAUNAY::TRIANGLE-CLASS (:name "tr-592" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-479" :coordinates #m(1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-476" :coordinates #m(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-475" :coordinates #m(0.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>))>
 #<DELAUNAY::TRIANGLE-CLASS (:name "tr-591" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-478" :coordinates #m(1.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-479" :coordinates #m(1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-475" :coordinates #m(0.000000000000000d0 -1.000000000000000d0 0.000000000000000d0))>))>
 #<DELAUNAY::TRIANGLE-CLASS (:name "tr-563" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-480" :coordinates #m(1.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-477" :coordinates #m(0.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-476" :coordinates #m(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>))>
 #<DELAUNAY::TRIANGLE-CLASS (:name "tr-562" :vertices (#<DELAUNAY::POINT-CLASS (:name "p-479" :coordinates #m(1.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-480" :coordinates #m(1.000000000000000d0 1.000000000000000d0 0.000000000000000d0))>
                                                       #<DELAUNAY::POINT-CLASS (:name "p-476" :coordinates #m(0.000000000000000d0 0.000000000000000d0 0.000000000000000d0))>))>)

## License

BSD2.0 (three clauses license)
