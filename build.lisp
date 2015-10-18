(require 'sb-posix)

;; (load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)
(ql:quickload :matrix-transforms)

(sb-ext:save-lisp-and-die "mt.bin"
                          :toplevel (lambda ()
                                      (sb-posix:putenv
                                       (format nil "SBCL_HOME=~A" 
                                               #.(sb-ext:posix-getenv "SBCL_HOME")))
                                      (matrix-transforms:game)
                                      0)
                          :executable t)

