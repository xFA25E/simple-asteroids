(asdf:defsystem "simple-asteroids"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cl-liballegro")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "types")
                 (:file "utils")
                 (:file "system")
                 (:file "init")
                 (:file "add")
                 (:file "collisions")
                 (:file "update")
                 (:file "render")
                 (:file "main"))))
  :description ""
  :around-compile
  (lambda (next)
    (proclaim '(optimize (compilation-speed 0) (debug 0) (safety 0) (space 3) (speed 3)))
    ;; (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))
    (funcall next)))
