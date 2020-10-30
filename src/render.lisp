(in-package :simple-asteroids)

(defvar *white-color* (al:map-rgb-f 1 1 1))
(defvar *black-color* (al:map-rgb-f 0 0 0))

(defun render-ship (ship)
  (declare (ship ship))
  (let ((nose (nose ship))
        (right-wing (right-wing ship))
        (left-wing (left-wing ship)))
    (al:draw-filled-triangle
     (x nose) (y nose)
     (x right-wing) (y right-wing)
     (x left-wing) (y left-wing)
     *white-color*)))

(defun render-asteroids (asteroids)
  (declare ((simple-array asteroid) asteroids))
  (loop :for asteroid :across asteroids
        :when (usedp asteroid) :do
          (al:draw-filled-circle
           (x asteroid) (y asteroid) (radius asteroid)
           *white-color*)))

(defun render-shots (shots)
  (declare ((simple-array shot) shots))
  (loop :for shot :across shots
        :when (usedp shot) :do
          (al:draw-filled-circle
           (x shot) (y shot) (radius shot)
           *white-color*)))
