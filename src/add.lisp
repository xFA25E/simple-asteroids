(in-package :simple-asteroids)

(declaim
 (inline add-explosion add-shot add-asteroid)
 (ftype (function (double-float double-float) (values null &optional)) add-explosion)
 (ftype (function nil (values null &optional)) add-shot)
 (ftype (function (asteroid-initials) (values null &optional)) add-asteroid))

(defun add-explosion (x y)
  (al:play-sample
   (ecase (random 2)
     (0 *explosion-1-sample*)
     (1 *explosion-2-sample*))
   0.3 0 1.0 :once (null-pointer))

  (loop :for explosion :of-type explosion :across *explosions*
        :unless (explosion-usedp explosion) :do
          (setf (x explosion) x
                (y explosion) y
                (explosion-usedp explosion) t
                (explosion-duration explosion) +explosion-stage-1-start+)

          (return-from add-explosion))
  nil)

(defun add-shot ()
  (when (zerop (mod *frames* +shot-frequency+))
    (loop :for shot :of-type shot :across *shots*
          :unless (usedp shot) :do
            (setf (x shot) *ship-nose-x*
                  (y shot) *ship-nose-y*
                  (dx shot) (* +shot-speed+ (cos *ship-direction*))
                  (dy shot) (* +shot-speed+ (sin *ship-direction*))
                  (radius shot) +shot-radius+
                  (duration shot) +shot-duration+
                  (usedp shot) t)
            (return-from add-shot nil)))
  nil)

(defun add-asteroid (initials)
  (when (zerop (mod *frames* (asteroid-frequency initials)))
    (loop :for asteroid :of-type asteroid :across *asteroids*
          :unless (usedp asteroid) :do
            (let ((direction (random +two-pi+))
                  (speed (asteroid-speed initials))
                  (radius (asteroid-radius initials)))
              (declare (type radiants direction) (type display-float speed radius))
              (setf (dx asteroid) (* speed (cos direction))
                    (dy asteroid) (* speed (sin direction))
                    (radius asteroid) radius
                    (radius-squared asteroid) (expt radius 2)
                    (usedp asteroid) t
                    (x-scale asteroid) (* radius +asteroid-bitmap-half-width-reciprocal+)
                    (y-scale asteroid) (* radius +asteroid-bitmap-half-height-reciprocal+))
              (case (random 2)
                (0 (setf (x asteroid) (random +display-width-d0+)
                         (y asteroid) 0d0))
                (1 (setf (x asteroid) 0d0
                         (y asteroid) (random +display-height-d0+))))
              (return-from add-asteroid nil))))
  nil)
