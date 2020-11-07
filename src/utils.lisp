(in-package :simple-asteroids)

(declaim
 (inline sorted-insert asset)
 (type radiants +two-pi+ +half-pi+)
 (ftype (function (highscore highscores) (values null &optional)) sorted-insert)
 (ftype (function (string) (values string &optional)) asset))

(defconstant +two-pi+ (* 2 (coerce pi 'double-float)))
(defconstant +half-pi+ (/ (coerce pi 'double-float) 2))

(define-modify-macro mulf (multiplicand) *)
(defmacro modincf (place increment modulo)
  (multiple-value-bind (temp-vars temp-vals vars set-form get-form)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list temp-vars temp-vals)
            (,(first vars) (mod (+ ,get-form ,increment) ,modulo)))
       ,set-form)))

(defmacro with-pointer ((var expression cleanup) &body body)
  `(let ((,var ,expression))
     (declare (type foreign-pointer ,var))
     (when (null-pointer-p ,var)
       (error ,(format nil "Error initializing pointer: (~A ~A ~A)" var expression cleanup)))
     (unwind-protect ,(if (cdr body)
                          (cons 'progn body)
                          (car body))
       (,cleanup ,var))))

(defmacro with-bitmap ((var expression) &body body)
  `(with-pointer (,var ,expression al:destroy-bitmap) ,@body))

(defmacro with-font ((var expression) &body body)
  `(with-pointer (,var ,expression al:destroy-font) ,@body))

(defmacro with-sample ((var expression) &body body)
  `(with-pointer (,var ,expression al:destroy-sample) ,@body))

(defmacro with-pointers (((kind variable expression) &rest binds) &body body)
  `(,(ecase kind (:bitmap 'with-bitmap) (:font 'with-font) (:sample 'with-sample))
    (,variable ,expression)
    ,@(if binds
          (list `(with-pointers ,binds ,@body))
          body)))

(defun sorted-insert (element array)
  (let ((position (position element array :test #'>)))
    (declare (type (or null fixnum) position))
    (when position
      (setf (subseq array (1+ position)) (subseq array position)
            (aref array position) element)))
  nil)

(defun asset (path)
  (namestring (asdf:system-relative-pathname "simple-asteroids" (concatenate 'string "assets/" path))))
