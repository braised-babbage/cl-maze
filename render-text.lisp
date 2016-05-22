(in-package :cl-maze)

;;; The constants should be odd, so that the cell has a "center".
(defconstant cell-text-width 5)
(defconstant cell-text-height 3)
(defconstant width-offset (- cell-text-width 1))
(defconstant height-offset (- cell-text-height 1))


(defun make-numeric-descriptor (cell-to-number)
  "Given a function mapping cells to numbers,
return a function mapping cells to characters."
  (lambda (c)
    (elt "0123456789abcdefghijklmnopqrstuvwxyz"
	 (funcall cell-to-number c))))


;;; A text-buffer is a 2D array of strings,
;;; with origin (0,0) corresponding to the
;;; upper left corner of a region of text.
(defun make-text-buffer (width height)
  (make-array (list width height)
	      :initial-element " "))

(defun render-grid-text (grid &key (stream t) (cell-descriptor nil))
  "Renders the grid in text format to the stream. Cell-descriptor
is either NIL or a function that maps from cells to characters. When
provided, cell-descriptor is used to determine the value at the center
of each cell's corresponding region in the text-buffer."
  ;; Cells intersect along their boundaries, so we need
  ;; to compute the buffer size accordingly.
  (let* ((buffer-width (+ (* (grid-width grid) (- cell-text-width 1)) 1))
	 (buffer-height (+ (* (grid-height grid) (- cell-text-height 1)) 1))
	 (text-buffer (make-text-buffer buffer-width buffer-height)))
    (grid-for-each (grid current-cell)
        (render-cell-text current-cell cell-descriptor text-buffer))
    (print-text-buffer text-buffer stream)))

(defun print-text-buffer (text-buffer stream)
  "Prints the text-buffer to stream."
  (let ((w (array-dimension text-buffer 0))
	(h (array-dimension text-buffer 1)))
    (dotimes (j h)
      (dotimes (i w)
	(format stream "~a" (aref text-buffer i j)))
      (format stream "~%"))))

(defun render-cell-text (cell cell-descriptor text-buffer)
  "Renders the cell in text format to the text-buffer. 
The cell-descriptor determines what character is at the center
of each cell's textual representation."
  ;; The upper-left corner of the cell
  (let ((x-base (* (cell-x cell) width-offset))
	(y-base (* (cell-y cell) height-offset)))
    ;; When two neighbors are not linked, a line segment is drawn between them.
    (unless (linkedp cell (cell-south cell))
      (render-horizontal-segment (cons x-base (+ y-base height-offset))
				 cell-text-width text-buffer))
    (unless (linkedp cell (cell-north cell))
      (render-horizontal-segment (cons x-base y-base)
				 cell-text-width text-buffer))
    (unless (linkedp cell (cell-west cell))
      (render-vertical-segment (cons x-base y-base)
			       cell-text-height text-buffer))
    (unless (linkedp cell (cell-east cell))
      (render-vertical-segment (cons (+ x-base width-offset) y-base)
			       cell-text-height text-buffer))
    (when cell-descriptor
      (render-char (funcall cell-descriptor cell)
		   (+ x-base (/ width-offset 2)) (+ y-base (/ height-offset 1)) text-buffer))))

(defun render-char (char x y text-buffer)
  "Renders a character at the x,y position of text-buffer."
  (setf (aref text-buffer x y) (string char)))

(defun render-horizontal-segment (point length text-buffer)
  "Renders a horizontal line segment starting at point,
and heading `east' for length characters."
  (let* ((x (car point))
	 (y (cdr point))
	 (x-final (+ x length -1)))
    (setf (aref text-buffer x y) "+")
    (loop for i from (+ x 1) below x-final
       do (setf (aref text-buffer i y) "-"))
    (setf (aref text-buffer x-final y) "+")))

(defun render-vertical-segment (point length text-buffer)
  "Renders a horizontal line segment starting at point,
and heading `south' for length characters."
  (let* ((x (car point))
	 (y (cdr point))
	 (y-final (+ y length -1)))
    (setf (aref text-buffer x y) "+")
    (loop for j from (+ y 1) below y-final
       do (setf (aref text-buffer x j) "|"))
    (setf (aref text-buffer x y-final) "+")))


(defun render-cell-corners (cell text-buffer)
  "Renders the cell corners to text-buffer."
  (let ((x-b (* (cell-x cell) width-offset))
	(y-b (* (cell-y cell) height-offset)))
    (setf (aref text-buffer x-b y-b) "+")
    (setf (aref text-buffer (+ x-b width-offset) y-b) "+")
    (setf (aref text-buffer x-b (+ y-b height-offset)) "+")
    (setf (aref text-buffer (+ x-b width-offset) (+ y-b height-offset)) "+")))
