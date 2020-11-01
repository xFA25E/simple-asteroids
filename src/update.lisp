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

  (unless (eq :released (right controls))
    (incf (direction ship) (direction-speed ship)))

  (unless (eq :released (left controls))
    (decf (direction ship) (direction-speed ship)))

  (let* ((direction (direction ship))
         (direction-x (cos direction))
         (direction-y (sin direction)))

    (unless (eq :released (up controls))
      (let ((speed (thruster-speed ship)))
        (incf (dx ship) (* speed direction-x))
        (incf (dy ship) (* speed direction-y))))

    (unless (eq :released (down controls))
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

(defun update-controls (controls)
  (declare (controls controls))
  (when (eq :pressed (up controls))
    (setf (up controls) :keep))
  (when (eq :pressed (down controls))
    (setf (down controls) :keep))
  (when (eq :pressed (left controls))
    (setf (left controls) :keep))
  (when (eq :pressed (right controls))
    (setf (right controls) :keep))
  (when (eq :pressed (enter controls))
    (setf (enter controls) :keep))
  (when (eq :pressed (escape controls))
    (setf (escape controls) :keep)))

(defun update-game (sys)
  (declare (system sys))
  (if (eq :pressed (escape (controls sys)))
      (setf (state sys) :game-over)
      (let ((frames (frames sys))
            (ship (ship sys)) (shots (shots sys)) (asteroids (asteroids sys))
            (width (al:width sys)) (height (al:height sys)))

        (update-ship ship width height (controls sys))
        (update-shots shots width height)
        (update-asteroids asteroids width height)

        (add-shot shots ship (shot-option sys) frames)
        (loop :for option :across (asteroid-options sys) :do
          (add-asteroid asteroids width height option frames))

        (loop :for asteroid :across asteroids
              :when (usedp asteroid) :do
                (loop :for shot :across shots
                      :when (and (usedp shot) (collide-shot-asteroid-p shot asteroid)) :do
                        (setf (usedp shot) nil (usedp asteroid) nil))
              :when (and (usedp asteroid) (collide-ship-asteroid-p ship asteroid))
                :do (setf (state sys) :game-over)
                :and :do (loop-finish))

        (incf (frames sys)))))

(defun update-menu (sys)
  (declare (system sys))
  (cond
    ((eq :pressed (up (controls sys)))
     (setf (menu-index sys) (mod (1+ (menu-index sys)) (length (menu sys)))))
    ((eq :pressed (down (controls sys)))
     (setf (menu-index sys) (mod (1- (menu-index sys)) (length (menu sys)))))
    ((eq :pressed (enter (controls sys)))
     (setf (state sys) (ecase (menu-index sys)
                         (0 (init-game sys) :game)
                         (1 :end)
                         (2 :highscores))))))

(defun update-game-over (sys)
  (declare (system sys))
  (let ((controls (controls sys)))
    (when (or (eq :pressed (enter controls))
              (eq :pressed (escape controls)))
      (setf (state sys) :menu))))

(defun update-highscores (sys)
  (declare (system sys))
  (setf (state sys) :menu))
