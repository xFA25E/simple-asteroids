(in-package :simple-asteroids)

(defun update-asteroids (asteroids width height)
  (declare ((simple-array asteroid) asteroids) ((integer 1 4000) width height))
  (loop :for asteroid :across asteroids
        :when (usedp asteroid) :do
          (setf (x asteroid) (mod (+ (x asteroid) (dx asteroid)) width)
                (y asteroid) (mod (+ (y asteroid) (dy asteroid)) height))))

(defun update-shots (shots width height)
  (declare ((simple-array shot) shots) ((integer 1 4000) width height))
  (loop :for shot :across shots
        :when (usedp shot) :do
          (if (plusp (decf (duration shot)))
              (setf (x shot) (mod (+ (x shot) (dx shot)) width)
                    (y shot) (mod (+ (y shot) (dy shot)) height))
              (setf (usedp shot) nil))))

(defun update-ship (ship width height controls)
  (declare (ship ship) ((integer 1 4000) width height) (controls controls))

  (let ((coefficient (brakes-coefficient ship)))
    (setf (dx ship) (* coefficient (dx ship))
          (dy ship) (* coefficient (dy ship))))

  (when (rightp controls)
    (incf (direction ship) (direction-speed ship)))

  (when (leftp controls)
    (decf (direction ship) (direction-speed ship)))

  (let* ((direction (direction ship))
         (direction-x (cos direction))
         (direction-y (sin direction)))

    (when (upp controls)
      (let ((speed (thruster-speed ship)))
        (incf (dx ship) (* speed direction-x))
        (incf (dy ship) (* speed direction-y))))

    (when (downp controls)
      (let ((speed (thruster-speed ship)))
        (decf (dx ship) (* speed direction-x))
        (decf (dy ship) (* speed direction-y))))

    (let* ((x (setf (x ship) (mod (+ (x ship) (dx ship)) width)))
           (y (setf (y ship) (mod (+ (y ship) (dy ship)) height)))
           (nose-length (nose-length ship))
           (wing-length (wing-length ship))
           (nose-dx (* nose-length direction-x))
           (nose-dy (* nose-length direction-y))
           (back-x (- x (/ nose-dx 2)))
           (back-y (- y (/ nose-dy 2)))
           (right-wing-direction (- direction +half-pi+))
           (right-wing-dx (* wing-length (cos right-wing-direction)))
           (right-wing-dy (* wing-length (sin right-wing-direction)))
           (nose (nose ship))
           (right-wing (right-wing ship))
           (left-wing (left-wing ship)))

      (setf (x nose) (+ x nose-dx)
            (y nose) (+ y nose-dy)
            (x right-wing) (+ back-x right-wing-dx)
            (y right-wing) (+ back-y right-wing-dy)
            (x left-wing) (- back-x right-wing-dx)
            (y left-wing) (- back-y right-wing-dy)))))
