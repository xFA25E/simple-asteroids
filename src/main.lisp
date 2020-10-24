(defpackage simple-asteroids
  (:use :cl)
  (:import-from #:cffi #:foreign-slot-value #:null-pointer-p #:foreign-enum-value))

(in-package :simple-asteroids)

(defconstant +deg-rad+ (/ al:+pi+ 180))
(defconstant +90-deg-rad+ (* 90 +deg-rad+))

(defconstant +display-width+ 800)
(defconstant +display-height+ 600)

(defconstant +ship-length+ 30)
(defconstant +ship-wing-length+ 10)
(defconstant +ship-angular-speed+ 5)

(defconstant +shot-size+ 2)
(defconstant +shot-speed+ 20)
(defconstant +shot-life+ 90)

(defconstant +asteroid-size+ 10)
(defconstant +asteroid-max-speed+ 5)

(defvar *done* nil)
(defvar *font* nil)
(defvar *game-over-font* nil)
(defvar *frames* 0)
(defvar *shots* nil)
(defvar *asteroids* nil)

(defvar *pressed-up-p* nil)
(defvar *pressed-down-p* nil)
(defvar *pressed-left-p* nil)
(defvar *pressed-right-p* nil)

(defvar *ship-x* 0)
(defvar *ship-y* 0)
(defvar *ship-angle* 45)
(defvar *ship-speed* 10)

(defclass shot ()
  ((x :initarg :x)
   (y :initarg :y)
   (dx :initarg :dx)
   (dy :initarg :dy)
   (life :initarg :life :initform +shot-life+)))

(defclass asteroid ()
  ((x :initarg :x :accessor asteroid-x)
   (y :initarg :y :accessor asteroid-y)
   (dx :initarg :dx)
   (dy :initarg :dy)))

(defclass simple-asteroids (al:system)
  ()
  (:default-initargs :title "Simple-Asteroids"
                     :width +display-width+ :height +display-height+
                     :display-options '((:sample-buffers 1 :suggest)
                                        (:samples 8 :suggest))))

(defun spawn-shot ()
  (let* ((ship-radiants (* *ship-angle* +deg-rad+))
         (dx (* +shot-speed+ (cos ship-radiants)))
         (dy (* +shot-speed+ (sin ship-radiants))))
    (push (make-instance 'shot :x *ship-x* :y *ship-y* :dx dx :dy dy) *shots*)))

(defun spawn-asteroid ()
  (push (case (random 2)
          (0 (make-instance 'asteroid
                            :x (random (1+ +display-width+))
                            :y 0
                            :dx (random +asteroid-max-speed+)
                            :dy (random +asteroid-max-speed+)))
          (1 (make-instance 'asteroid
                            :x 0
                            :y (random (1+ +display-height+))
                            :dx (random +asteroid-max-speed+)
                            :dy (random +asteroid-max-speed+))))
        *asteroids*))

(defmethod al:system-loop :before ((sys simple-asteroids))
  (setf *font* (al:create-builtin-font))
  (when (null-pointer-p *font*)
    (error "Cant create builtin font"))

  (setf *game-over-font* (al:load-ttf-font "/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf" 46 0))
  (when (null-pointer-p *game-over-font*)
    (al:destroy-font *font*)
    (error "Cant find monospcaed font")))

(defmethod al:system-loop :after ((sys simple-asteroids))
  (al:destroy-font *font*))

(defmethod al:key-down-handler ((sys simple-asteroids))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (setf *pressed-up-p* t))
    (:down (setf *pressed-down-p* t))
    (:left (setf *pressed-left-p* t))
    (:right (setf *pressed-right-p* t))))

(defmethod al:key-up-handler ((sys simple-asteroids))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (setf *pressed-up-p* nil))
    (:down (setf *pressed-down-p* nil))
    (:left (setf *pressed-left-p* nil))
    (:right (setf *pressed-right-p* nil))
    (:escape (setf (al:system-loop-running-p sys) nil))))

(defmethod al:update ((sys simple-asteroids))
  (unless *done*
    (when *pressed-right-p*
      (incf *ship-angle* +ship-angular-speed+))
    (when *pressed-left-p*
      (decf *ship-angle* +ship-angular-speed+))
    (when *pressed-up-p*
      (let ((ship-radiants (* *ship-angle* +deg-rad+)))
        (setf *ship-x* (mod (+ *ship-x* (* *ship-speed* (cos ship-radiants))) +display-width+)
              *ship-y* (mod (+ *ship-y* (* *ship-speed* (sin ship-radiants))) +display-height+))))
    (when *pressed-down-p*
      (let ((ship-radiants (* *ship-angle* +deg-rad+)))
        (setf *ship-x* (mod (- *ship-x* (* *ship-speed* (cos ship-radiants))) +display-width+)
              *ship-y* (mod (- *ship-y* (* *ship-speed* (sin ship-radiants))) +display-height+))))

    (when (zerop (mod *frames* 10))
      (spawn-shot))

    (when (zerop (mod *frames* 60))
      (spawn-asteroid))

    (loop :for asteroid :in *asteroids* :do
      (with-slots (x y dx dy) asteroid
        (setf x (mod (+ x dx) +display-width+)
              y (mod (+ y dy) +display-height+))))

    (setf *shots* (delete-if
                   (lambda (shot)
                     (block lambda
                       (with-slots (x y dx dy life) shot
                         (setf x (mod (+ x dx) +display-width+)
                               y (mod (+ y dy) +display-height+))
                         (when (minusp (decf life 1))
                           (return-from lambda t))

                         (let ((collidep nil))
                           (setf *asteroids*
                                 (delete-if
                                  (lambda (asteroid)
                                    (let ((ax (asteroid-x asteroid))
                                          (ay (asteroid-y asteroid)))
                                      (when (< (+ (expt (- x ax) 2) (expt (- y ay) 2))
                                               (expt (+ +shot-size+ +asteroid-size+) 2))
                                        (setf collidep t))))
                                  *asteroids*))
                           collidep))))
                   *shots*))

    (loop :for asteroid :in *asteroids* :do
      (with-slots (x y) asteroid
        (when (< (+ (expt (- *ship-x* x) 2) (expt (- *ship-y* y) 2))
                 (expt (/ +ship-length+ 2) 2))
          (setf *done* t))))

    (incf *frames*)))

(defmethod al:render ((sys simple-asteroids))
  (al:clear-to-color (al:map-rgb 0 0 0))
  (al:draw-text *font* (al:map-rgb-f 1 1 1)
                1 1
                (foreign-enum-value 'al::align-flags :left)
                (format nil "fms: ~d x: ~d y: ~d a: ~d s: ~d" *frames* *ship-x* *ship-y* *ship-angle* *ship-speed*))

  (let* ((ship-radiants (* *ship-angle* +deg-rad+))
         (nose-dx (* +ship-length+ (cos ship-radiants)))
         (nose-dy (* +ship-length+ (sin ship-radiants)))
         (nose-x (+ *ship-x* nose-dx))
         (nose-y (+ *ship-y* nose-dy))
         (back-x (- *ship-x* (/ nose-dx 2)))
         (back-y (- *ship-y* (/ nose-dy 2)))
         (right-wing-dx (* +ship-wing-length+ (cos (- ship-radiants +90-deg-rad+))))
         (right-wing-dy (* +ship-wing-length+ (sin (- ship-radiants +90-deg-rad+))))
         (right-wing-x (+ back-x right-wing-dx))
         (right-wing-y (+ back-y right-wing-dy))
         (left-wing-x (- back-x right-wing-dx))
         (left-wing-y (- back-y right-wing-dy)))
    (al:draw-filled-triangle nose-x nose-y right-wing-x right-wing-y left-wing-x left-wing-y (al:map-rgb-f 1 1 1))
    (al:draw-circle *ship-x* *ship-y* (/ +ship-length+ 2) (al:map-rgb-f 1 1 1) 2))

  (loop :for asteroid :in *asteroids* :do
    (with-slots (x y) asteroid
      (al:draw-filled-circle x y +asteroid-size+ (al:map-rgb-f 1 1 1))))

  (loop :for shot :in *shots* :do
    (with-slots (x y life) shot
      (when (plusp life)
        (al:draw-filled-circle x y +shot-size+ (al:map-rgb-f 1 1 1)))))

  (when *done*
    (al:draw-text *game-over-font* (al:map-rgb-f 1 1 1)
                  (/ +display-width+ 2) (/ +display-height+ 2)
                  (foreign-enum-value 'al::align-flags :center)
                  "G A M E  O V E R"))

  (al:flip-display))

(defun main ()
  (let ((system (make-instance 'simple-asteroids)))
    (setf *done* nil

          *frames* 0
          *asteroids* nil
          *shots* nil

          *firedp* nil
          *pressed-up-p* nil
          *pressed-down-p* nil
          *pressed-left-p* nil
          *pressed-right-p* nil

          *ship-x* (/ (al:width system) 2)
          *ship-y* (/ (al:height system) 2)
          *ship-angle* 90
          *ship-speed* 10)

    (al:run-system system)))
