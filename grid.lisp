;;; grid.lisp
;;;
;;; Copyright 2016 Erik Davis

;(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :cl-maze)

;;; Cells

(defstruct cell
  "A cell has a spatial position, as well as neighbors."
  x y north south east west links)

(defun cell-neighbors (cell)
    "Returns the list of neighboring cells."
    (remove nil (list (cell-north cell)
		      (cell-south cell)
		      (cell-east cell)
		      (cell-west cell))))

(defun link-cell (cell neighbor &optional (bidirectional t))
  "Links cell to neighbor. The link may be bidirectional or
one-way. When neighbor is null, do nothing."
  (unless (null neighbor)
    (setf (cell-links cell) (cons neighbor (cell-links cell)))
    (when bidirectional (link-cell neighbor cell nil)))
  cell)

(defun unlink-cell (cell neighbor &optional (bidirectional t))
  "Unlinkes neighbor from cell. May be used to remove bidirectional liks."
  (setf (cell-links cell) (remove neighbor (cell-links cell)))
  (when bidirectional (unlink-cell neighbor cell nil))
  cell)

(defun linkedp (cell neighbor)
  "Is cell linked to neighbor?"
  (member neighbor (cell-links cell)))

;;; Grids

(defstruct (grid (:print-function print-grid))
  "A grid is a regular collection of cells."
  width height cells)

(defun print-grid (grid &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Grid :width ~a :height ~a>"
	  (grid-width grid) (grid-height grid)))

(defmacro grid-for-each (args &rest body)
  "Usage: (grid-for-each (grid cell) <body>)
   Loops over the cells of grid, with cell
   denoting the current grid cell in the execution
   of <body>." 
  (let ((grid (first args))
	(cell (second args)))
    (let ((i (gensym))
	  (j (gensym))
	  (w (gensym))
	  (h (gensym)))
      `(let ((,w (grid-width ,grid))
	     (,h (grid-height ,grid)))
	 (dotimes (,i ,w)
	   (dotimes (,j ,h)
	     (let ((,cell (grid-ref ,grid ,i ,j)))
	       ,@body)))))))

(defun grid-ref (grid i j)
  "Returns the cell at location (i,j), 
or NIL when the indices are out of range."
  (if (and (<= 0 i) (< i (grid-width grid))
	   (<= 0 j) (< j (grid-height grid)))
      (aref (grid-cells grid) i j)
      nil))

(defun grid-cell-list (grid)
  (let ((cells (list)))
    (grid-for-each (grid cell)
		   (push cell cells))
    cells))

(defun build-and-initialize-grid (width height)
  "Makes a regular grid with specified width and height."
  (let ((g (make-grid :width width :height height)))
    (fill-cells g)
    (connect-cells g)
    g))

(defun fill-cells (grid)
  "Fills the grid with cells."
  (let ((w (grid-width grid))
	(h (grid-height grid)))
    (setf (grid-cells grid) (make-array (list w h)))
    (dotimes (i w)
      (dotimes (j h)
	(setf (aref (grid-cells grid) i j)
	      (make-cell :x i :y j))))))

(defun connect-cells (grid)
  "Connects the cells in grid to their neighbors."
  (grid-for-each (grid cell)
    (let ((i (cell-x cell))
	  (j (cell-y cell)))
      (setf (cell-north cell) (grid-ref grid i (- j 1)))
      (setf (cell-south cell) (grid-ref grid i (+ j 1)))
      (setf (cell-west cell) (grid-ref grid (- i 1) j))
      (setf (cell-east cell) (grid-ref grid (+ i 1) j)))))

(defun random-cell (grid)
  "Selects a random cell from grid."
  (grid-ref grid
	    (random (grid-width grid))
	    (random (grid-height grid))))

(defun select-random (list)
  (let ((rlist (remove nil list)))
    (if (null rlist)
	nil
	(elt rlist (random (length rlist))))))




;;; This stuff doesn't belong here!!!

(defun distance-function (grid origin)
  (let ((dtable (make-hash-table)))
    (fill-distance-table grid (list (cons origin 0)) dtable)
    (lambda (cell)
      (gethash cell dtable))))

(defun fill-distance-table (grid frontier dtable)
  (unless (null frontier)
    (let* ((state (pop frontier))
	   (cell (car state))
	   (dist (cdr state)))
      (fill-distance-table
       grid
       (if (gethash cell dtable)
	   frontier
	   (update-frontier frontier cell dist dtable))
       dtable))))

(defun update-frontier (frontier cell dist dtable)
  (setf (gethash cell dtable) dist)
  (let ((new-states (mapcar #'(lambda (c) (cons c (+ dist 1)))
			    (cell-links cell))))
    (append frontier new-states)))


