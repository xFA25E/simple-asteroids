(asdf:defsystem "simple-asteroids"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cl-liballegro" "alexandria")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "types")
                 (:file "add")
                 (:file "update")
                 (:file "collisions")
                 (:file "render")
                 (:file "main"))))
  :description ""
  :around-compile
  (lambda (next)
    (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))
    (funcall next)))
