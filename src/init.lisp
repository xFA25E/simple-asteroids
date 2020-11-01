(in-package :simple-asteroids)

(defun init-ship (ship width height option)
  (declare (ship ship) ((integer 1 4000) width height) (ship-option option))
  (setf (x ship) (/ width 2d0)
        (y ship) (/ height 2d0)
        (dx ship) (ship-option-dx option)
        (dy ship) (ship-option-dy option)
        (direction ship) (ship-option-direction option)))

(defun init-asteroids (asteroids)
  (declare ((simple-array asteroid) asteroids))
  (loop :for asteroid :across asteroids :do
    (setf (usedp asteroid) nil)))

(defun init-shots (shots)
  (declare ((simple-array shot) shots))
  (loop :for shot :across shots :do
    (setf (usedp shot) nil)))

(defun init-game (sys)
  (declare (system sys))
  (init-ship (ship sys) (al:width sys) (al:height sys) (ship-option sys))
  (init-asteroids (asteroids sys))
  (init-shots (shots sys)))
