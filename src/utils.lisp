(in-package :simple-asteroids)

(defconstant +two-pi+ (* 2 pi))
(defconstant +half-pi+ (/ pi 2))

(defmacro with-font ((font expression) &body body)
  `(let ((,font ,expression))
     (when (null-pointer-p ,font)
       (error "Error initializing font"))
     (unwind-protect (progn ,@body)
       (al:destroy-font ,font))))

(defmacro with-fonts (binds &body body)
  (if (null binds)
      `(progn ,@body)
      (destructuring-bind (variable expression) (first binds)
        `(with-font (,variable ,expression)
           (with-fonts ,(rest binds)
             ,@body)))))
