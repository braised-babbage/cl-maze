(in-package :cl-maze)

(defun north-east-maze (width height)
  "Generates a maze of given width and height by a simple heuristic:
each cell connects with a random choice of its north or east neighbor,
when these exist."
  (let ((grid (build-and-initialize-grid width height)))
    (grid-for-each (grid cell)
      (link-cell
       cell
       (select-random (list (cell-north cell)
			    (cell-east cell)))))
    grid))

(defun aldous-broder-maze (width height)
  "Generates a maze of given width and height by generating a
random spanning tree on the underlying grid. The sampling algorithm
is described in 

Broder, Andrei. \"Generating random spanning trees.\" 1989
"
  (let* ((grid (build-and-initialize-grid width height))
	 (cell (random-cell grid))
	 (next nil)
	 (unvisited (grid-cell-list grid)))
    ;; Walk randomly through the grid, and whenever you reach
    ;; an unvisited cell, add a link for the incoming edge.
    ;; Stop when all cells are visited.
    (loop while unvisited do
	 (setf next (select-random (cell-neighbors cell)))
	 (when (member next unvisited)
	   (link-cell cell next))
	 (setf unvisited (remove next unvisited))
	 (setf cell next))
    grid))
