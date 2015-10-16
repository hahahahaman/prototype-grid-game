;;;; matrix-transforms.asd

(asdf:defsystem #:matrix-transforms
  :description "puzzling"
  :author "Ed Ye <hahahadude@gmail.com>"
  :license "licenseless Rider"
  :depends-on (#:alexandria
               #:cl-glfw3
               #:cl-opengl
               #:cl-soil
               #:fset
               #:glkit
               #:trivial-garbage
               #:defenum
               #:swank)
  :serial t
  :components ((:file "package")
               (:file "matrix-transforms")))

