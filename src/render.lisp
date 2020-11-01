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

(defun render-game (sys)
  (declare (system sys))
  (al:clear-to-color *black-color*)
  (render-ship (ship sys))
  (render-shots (shots sys))
  (render-asteroids (asteroids sys))
  (al:flip-display))

(defun render-menu (sys)
  (declare (system sys))
  (al:clear-to-color *black-color*)
  (al:draw-text (game-over-font sys) *white-color*
                (/ (al:width sys) 2) (/ (al:height sys) 2)
                (foreign-enum-value 'al::align-flags :center)
                (aref (menu sys) (menu-index sys)))
  (al:flip-display))

(defun render-game-over (sys)
  (declare (system sys))
  (al:clear-to-color *black-color*)
  (al:draw-text (game-over-font sys) *white-color*
                (/ (al:width sys) 2) (/ (al:height sys) 2)
                (foreign-enum-value 'al::align-flags :center)
                "G A M E  O V E R")
  (al:flip-display))

(defun render-highscores (sys)
  (declare (system sys))
  sys)
