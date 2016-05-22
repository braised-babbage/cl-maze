# cl-maze

CL-MAZE is a package for generating mazes and rendering them, either to text or as [svg images](https://cdn.rawgit.com/kilimanjaro/cl-maze/master/maze40x40.svg).

## Example
```
CL-USER> (asdf:load-system "cl-maze")
T
CL-USER> (in-package :cl-maze)
#<Package "CL-MAZE">
CL-MAZE> (render-grid-text (north-east-maze 10 10))
+---+---+---+---+---+---+---+---+---+---+
|                                       |
+   +   +   +---+---+   +---+---+   +   +
|   |   |   |           |           |   |
+   +---+   +   +---+   +---+   +   +   +
|   |       |   |       |       |   |   |
+---+---+   +---+   +   +   +---+   +   +
|           |       |   |   |       |   |
+---+   +---+   +---+---+   +---+   +   +
|       |       |           |       |   |
+---+---+   +---+   +---+---+   +   +   +
|           |       |           |   |   |
+---+   +---+---+---+---+   +   +---+   +
|       |                   |   |       |
+---+---+   +---+   +---+---+---+---+   +
|           |       |                   |
+   +   +   +   +   +---+---+   +   +   +
|   |   |   |   |   |           |   |   |
+---+   +---+   +---+   +   +   +---+   +
|       |       |       |   |   |       |
+---+---+---+---+---+---+---+---+---+---+
NIL
```
