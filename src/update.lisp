(in-package :simple-asteroids)

(declaim
 (inline update-explosions update-stars update-asteroids
         update-shots update-ship update-controls update-game
         update-menu update-game-over update-highscores)
 (ftype (function nil (values null &optional))
        update-explosions update-stars update-asteroids
        update-shots update-ship update-controls update-game
        update-menu update-game-over update-highscores))

(defun update-explosions ()
  (loop :for explosion :of-type explosion :across *explosions*
        :when (explosion-usedp explosion) :do
          (if (plusp (explosion-duration explosion))
              (decf (explosion-duration explosion))
              (setf (explosion-usedp explosion) nil)))
  nil)

(defun update-stars ()
  (loop :for star :of-type star :across *stars* :do
    (modincf (x star) (dx star) +display-width-d0+)
    (modincf (y star) (dy star) +display-height-d0+)
    (modincf (star-direction star) +star-rotation-speed+ +two-pi+))
  nil)

(defun update-asteroids ()
  (loop :for asteroid :of-type asteroid :across *asteroids*
        :when (usedp asteroid) :do
          (modincf (x asteroid) (dx asteroid) +display-width-d0+)
          (modincf (y asteroid) (dy asteroid) +display-height-d0+)
          (modincf (asteroid-direction asteroid) +asteroid-rotation-speed+ +two-pi+))
  nil)

(defun update-shots ()
  (loop :for shot :of-type shot :across *shots*
        :when (usedp shot)
          :if (plusp (decf (duration shot)))
            :do (modincf (x shot) (dx shot) +display-width-d0+)
                (modincf (y shot) (dy shot) +display-height-d0+)
        :else :do (setf (usedp shot) nil))
  nil)

(defun update-ship ()
  (mulf *ship-dx* +ship-brakes-coefficient+)
  (mulf *ship-dy* +ship-brakes-coefficient+)

  (unless (eq :released *key-right*)
    (modincf *ship-direction* +ship-rotation-speed+ +two-pi+))

  (unless (eq :released *key-left*)
    (modincf *ship-direction* (- +ship-rotation-speed+) +two-pi+))

  (let ((direction-x (cos *ship-direction*))
        (direction-y (sin *ship-direction*)))
    (declare (type (double-float -1d0 1d0) direction-x direction-y))

    (unless (eq :released *key-up*)
      (incf *ship-dx* (* +ship-thruster-power+ direction-x))
      (incf *ship-dy* (* +ship-thruster-power+ direction-y)))

    (unless (eq :released *key-down*)
      (decf *ship-dx* (* +ship-thruster-power+ direction-x))
      (decf *ship-dy* (* +ship-thruster-power+ direction-y)))

    (modincf *ship-x* *ship-dx* +display-width-d0+)
    (modincf *ship-y* *ship-dy* +display-height-d0+)

    (let* ((nose-dx (* +ship-half-nose-length+ direction-x))
           (nose-dy (* +ship-half-nose-length+ direction-y))
           (back-x (- *ship-x* nose-dx))
           (back-y (- *ship-y* nose-dy))
           (right-wing-direction (mod (- *ship-direction* +half-pi+) +two-pi+))
           (right-wing-dx (* +ship-wing-length+ (cos right-wing-direction)))
           (right-wing-dy (* +ship-wing-length+ (sin right-wing-direction))))
      (declare
       (type radiants right-wing-direction)
       (type display-float back-x back-y nose-dx nose-dy right-wing-dx right-wing-dy))

      (setf *ship-nose-x* (+ *ship-x* nose-dx)
            *ship-nose-y* (+ *ship-y* nose-dy)
            *ship-right-wing-x* (+ back-x right-wing-dx)
            *ship-right-wing-y* (+ back-y right-wing-dy)
            *ship-left-wing-x* (- back-x right-wing-dx)
            *ship-left-wing-y* (- back-y right-wing-dy))))
  nil)

(defun update-controls ()
  (when (eq :pressed *key-up*)
    (setf *key-up* :keep))
  (when (eq :pressed *key-down*)
    (setf *key-down* :keep))
  (when (eq :pressed *key-left*)
    (setf *key-left* :keep))
  (when (eq :pressed *key-right*)
    (setf *key-right* :keep))
  (when (eq :pressed *key-enter*)
    (setf *key-enter* :keep))
  (when (eq :pressed *key-escape*)
    (setf *key-escape* :keep))
  nil)

(defun update-game ()
  (if (eq :pressed *key-escape*)
      (setf *state* :game-over)
      (progn
        (update-ship)
        (update-shots)

        (add-shot)
        (loop :for asteroid-initials :of-type asteroid-initials :across *asteroid-initials* :do
          (add-asteroid asteroid-initials))

        (loop :for asteroid :of-type asteroid :across *asteroids*
              :when (usedp asteroid) :do
                (loop :for shot :of-type shot :across *shots*
                      :when (and (usedp shot) (collide-shot-asteroid-p shot asteroid)) :do
                        (setf (usedp shot) nil (usedp asteroid) nil)
                        (incf *current-highscore* (round (radius asteroid)))
                        (add-explosion (x asteroid) (y asteroid)))
              :when (and (usedp asteroid) (collide-ship-asteroid-p asteroid)) :do
                (setf *state* :game-over)
                (add-explosion *ship-x* *ship-y*)
                (loop-finish))

        (incf *frames*)))
  nil)

(defun update-menu ()
  (cond ((eq :pressed *key-up*)
         (modincf *menu-index* -1 (length *menu*)))
        ((eq :pressed *key-down*)
         (modincf *menu-index* 1 (length *menu*)))
        ((eq :pressed *key-enter*)
         (setf *state* (ecase *menu-index*
                         (0 (init-game) :game)
                         (1 :highscores)
                         (2 :end)))))
  nil)

(defun update-game-over ()
  (when (or (eq :pressed *key-enter*) (eq :pressed *key-escape*))
    (sorted-insert *current-highscore* *highscores*)
    (setf *state* :menu))
  nil)

(defun update-highscores ()
  (when (or (eq :pressed *key-enter*) (eq :pressed *key-escape*))
    (setf *state* :menu))
  nil)
