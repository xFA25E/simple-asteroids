(defpackage simple-asteroids
  (:use :cl)
  (:import-from #:alexandria #:random-elt)
  (:import-from #:cffi
                #:foreign-slot-value
                #:null-pointer-p
                #:null-pointer
                #:foreign-pointer
                #:foreign-enum-value))
