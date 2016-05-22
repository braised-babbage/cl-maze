(in-package :cl-maze)

(defparameter *svg-stream* nil)

(defconstant border-pixels 30)
(defconstant cell-width-pixels 20)
(defconstant cell-height-pixels 20)
(defconstant svg-line-style "stroke:rgb(0,0,0);stroke-width:2")


(defun render-grid-svg (grid &optional (stream t))
  "Renders the grid as an SVG image to stream."
  (let ((*svg-stream* stream)
	(svg-width (+ (* (+ (grid-width grid) 1) cell-width-pixels)
		      (* 2 border-pixels)))
	(svg-height (+ (* (+ (grid-width grid) 1) cell-height-pixels)
		       (* 2 border-pixels))))
    (with-html-output
	(*svg-stream* nil :prologue
	       	"<?xml version=\"1.0\" standalone=\"yes\"?> 

<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
		:indent T)
      (:svg :width (write-to-string svg-width)
	    :height (write-to-string svg-height)
	    :xmlns "http://www.w3.org/2000/svg" :|xmlns:xlink| "http://www.w3.org/1999/xlink"
	    (grid-for-each (grid cell)
			   (render-cell-svg cell))))))

(defun render-cell-svg (cell)
  "Renders cell to *svg-stream*"
  (let* ((x1 (+ (* (cell-x cell) cell-width-pixels) border-pixels))
	 (y1 (+ (* (cell-y cell) cell-height-pixels) border-pixels))
	 (x2 (+ x1 cell-width-pixels))
	 (y2 (+ y1 cell-height-pixels)))
    (unless (linkedp cell (cell-south cell))
      (svg-line x1 y2 x2 y2))
    (unless (linkedp cell (cell-north cell))
      (svg-line x1 y1 x2 y1))
    (unless (linkedp cell (cell-west cell))
      (svg-line x1 y1 x1 y2))
    (unless (linkedp cell (cell-east cell))
      (svg-line x2 y1 x2 y2))))

(defun svg-line (x1 y1 x2 y2)
  "Draws a line from (x1,y1) to (x2,y2), outputting SVG to *svg-stream*"
  (with-html-output (*svg-stream* nil :indent t)
    (:line :x1 (write-to-string x1) :y1 (write-to-string y1)
	   :x2 (write-to-string x2) :y2 (write-to-string y2)
	   :style svg-line-style)))
