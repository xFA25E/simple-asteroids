(in-package :simple-asteroids)

(defun add-shot (shots ship option frames)
  (declare ((simple-array shot) shots) (ship ship) (shot-option option) ((integer 0) frames))
  (when (zerop (mod frames (shot-option-frequency option)))
    (loop :for shot :across shots
          :unless (usedp shot) :do
            (let ((direction (direction ship))
                  (speed (shot-option-speed option)))
              (setf (x shot) (x ship)
                    (y shot) (y ship)
                    (dx shot) (* speed (cos direction))
                    (dy shot) (* speed (sin direction))
                    (radius shot) (shot-option-radius option)
                    (duration shot) (shot-option-duration option)
                    (usedp shot) t)
              (return-from add-shot)))))

(defun add-asteroid (asteroids width height option frames)
  (declare ((simple-array asteroid) asteroids) ((integer 1 4000) width height) (asteroid-option option)
           ((integer 0) frames))
  (when (zerop (mod frames (asteroid-option-frequency option)))
    (loop :for asteroid :across asteroids
          :unless (usedp asteroid) :do
            (let ((direction (random +two-pi+))
                  (speed (asteroid-option-speed option))
                  (radius (asteroid-option-radius option)))
              (setf (dx asteroid) (* speed (cos direction))
                    (dy asteroid) (* speed (sin direction))
                    (radius asteroid) radius
                    (radius-squared asteroid) (expt radius 2)
                    (usedp asteroid) t)
              (case (random-elt #(:vertical :horizontal))
                (:horizontal
                 (setf (x asteroid) (random (+ 1d0 width))
                       (y asteroid) 0d0))
                (:vertical
                 (setf (x asteroid) 0d0
                       (y asteroid) (random (+ 1d0 height)))))
              (return-from add-asteroid)))))
