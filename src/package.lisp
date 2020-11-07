(defpackage simple-asteroids
  (:use :cl)
  (:import-from #:cffi
                #:foreign-slot-value
                #:null-pointer-p
                #:null-pointer
                #:foreign-pointer
                #:foreign-free
                #:foreign-enum-value)
  (:export #:main))
