;;;; matrix-transforms.asd

(asdf:defsystem #:matrix-transforms
  :description "Describe matrix-transforms here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-glfw3
               #:cl-opengl
               #:cl-soil
               #:fset
               #:glkit
               #:trivial-garbage
               #:defenum)
  :serial t
  :components ((:file "package")
               (:file "matrix-transforms")))

