;;; cl-maze.asd
;;;
;;; Copyright (c) 2016 Erik Davis

(defsystem :cl-maze
  :description "cl-maze: code for generating and rendering mazes"
  :version "0.0.0"
  :author "Erik Davis <erik@cadlag.org>"
  :license "TODO"
  :depends-on ("cl-who")
  :serial t
  :components ((:file "package")
	       (:file "grid")
	       (:file "generators")
	       (:file "render-text")
	       (:file "render-svg")))
