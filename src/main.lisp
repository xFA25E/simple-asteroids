(in-package :simple-asteroids)

(declaim (ftype (function nil (values null &optional)) main))

(defmethod al:system-loop :around ((sys system))
  (al:reserve-samples 128)
  (with-pointers
      ((:font *menu-font* (al:load-ttf-font *font-asset* +menu-font-size+ 0))
       (:font *game-over-font* (al:load-ttf-font *font-asset* +game-over-font-size+ 0))
       (:font *highscores-font* (al:load-ttf-font *font-asset* +highscores-font-size+ 0))
       (:font *current-highscore-font* (al:load-ttf-font *font-asset* +current-highscore-font-size+ 0))
       (:bitmap *background-bitmap* (al:load-bitmap *background-bitmap-asset*))
       (:bitmap sheet-bitmap* (al:load-bitmap *sheet-bitmap-asset*))
       (:bitmap *asteroid-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 15 78 +asteroid-bitmap-width+ +asteroid-bitmap-height+))
       (:bitmap *ship-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 0 4 +ship-bitmap-width+ +ship-bitmap-height+))
       (:bitmap *fire-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 11 48 +fire-bitmap-width+ +fire-bitmap-height+))
       (:bitmap *star-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 47 58 +star-bitmap-width+ +star-bitmap-height+))
       (:bitmap *explosion-1-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 64 0 +explosion-1-bitmap-width+ +explosion-1-bitmap-height+))
       (:bitmap *explosion-2-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 64 60 +explosion-2-bitmap-width+ +explosion-2-bitmap-height+))
       (:sample *explosion-1-sample* (al:load-sample *explosion-1-audio-asset*))
       (:sample *explosion-2-sample* (al:load-sample *explosion-2-audio-asset*)))
    (call-next-method)))

(defmethod al:key-down-handler ((sys system))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (unless (eq :keep *key-up*) (setf *key-up* :pressed)))
    (:down (unless (eq :keep *key-down*) (setf *key-down* :pressed)))
    (:left (unless (eq :keep *key-left*) (setf *key-left* :pressed)))
    (:right (unless (eq :keep *key-right*) (setf *key-right* :pressed)))
    (:enter (unless (eq :keep *key-enter*) (setf *key-enter* :pressed)))
    (:escape (unless (eq :keep *key-escape*) (setf *key-escape* :pressed)))))

(defmethod al:key-up-handler ((sys system))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (setf *key-up* :released))
    (:down (setf *key-down* :released))
    (:left (setf *key-left* :released))
    (:right (setf *key-right* :released))
    (:enter (setf *key-enter* :released))
    (:escape (setf *key-escape* :released))))

(defmethod al:update ((sys system))
  (update-stars)
  (update-explosions)
  (update-asteroids)
  (ecase *state*
    (:game (update-game))
    (:menu (update-menu))
    (:game-over (update-game-over))
    (:highscores (update-highscores))
    (:start (setf *state* :menu))
    (:end (setf *state* :start (al:system-loop-running-p sys) nil)))
  (update-controls))

(defmethod al:render ((sys system))
  (al:draw-bitmap *background-bitmap* 0 0 0)
  (render-stars)
  (render-explosions)
  (render-asteroids)
  (ecase *state*
    (:game (render-game))
    (:menu (render-menu))
    (:game-over (render-game-over))
    (:highscores (render-highscores))
    ((:start :end)))
  (al:flip-display))

(defun main ()
  (al:run-system (make-instance 'system))
  nil)
