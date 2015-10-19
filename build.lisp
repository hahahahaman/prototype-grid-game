(require 'sb-posix)

(load #P"~/quicklisp/setup.lisp")

(ql:quickload :alexandria
              :cl-glfw3
              :cl-opengl
              :cl-soil
              :fset
              :glkit
              :trivial-garbage
              :defenum
              :swank)

(asdf:oos 'asdf:load-op 'matrix-transforms)
(sb-ext:save-lisp-and-die "mt.bin"
                          :toplevel (lambda ()
                                      (sb-posix:putenv
                                       (format nil "SBCL_HOME=~A"
                                               #.(sb-ext:posix-getenv "SBCL_HOME")))
                                      (matrix-transforms:game)
                                      0)
                          :executable t)

