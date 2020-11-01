(in-package :simple-asteroids)

(defmethod al:system-loop :around ((sys system))
  (with-font (game-over
              (al:load-ttf-font
               "/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf" 46 0))
    (with-slots (game-over-font) sys
      (setf game-over-font game-over))
    (call-next-method)))

(defmethod al:key-down-handler ((sys system))
  (let ((controls (controls sys)))
    (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
      (:up (unless (eq :keep (up controls))
             (setf (up controls) :pressed)))
      (:down (unless (eq :keep (down controls))
               (setf (down controls) :pressed)))
      (:left (unless (eq :keep (left controls))
               (setf (left controls) :pressed)))
      (:right (unless (eq :keep (right controls))
                (setf (right controls) :pressed)))
      (:enter (unless (eq :keep (enter controls))
                (setf (enter controls) :pressed)))
      (:escape (unless (eq :keep (escape controls))
                 (setf (escape controls) :pressed))))))

(defmethod al:key-up-handler ((sys system))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (setf (up (controls sys)) :released))
    (:down (setf (down (controls sys)) :released))
    (:left (setf (left (controls sys)) :released))
    (:right (setf (right (controls sys)) :released))
    (:enter (setf (enter (controls sys)) :released))
    (:escape (setf (escape (controls sys)) :released))))

(defmethod al:update ((sys system))
  (ecase (state sys)
    (:game (update-game sys))
    (:menu (update-menu sys))
    (:game-over (update-game-over sys))
    (:highscores (update-highscores sys))
    (:start (setf (state sys) :menu))
    (:end (setf (al:system-loop-running-p sys) nil)))

  (update-controls (controls sys)))

(defmethod al:render ((sys system))
  (ecase (state sys)
    (:game (render-game sys))
    (:menu (render-menu sys))
    (:game-over (render-game-over sys))
    (:highscores (render-highscores sys))
    ((:start :end))))

(defun main ()
  (al:run-system (make-instance 'system)))
