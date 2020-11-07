(in-package :simple-asteroids)

(declaim
 (inline init-ship init-asteroids init-shots init-game)
 (ftype (function nil (values null &optional)) init-ship init-asteroids init-shots init-game))

(defun init-ship ()
  (setf *ship-x* +display-half-width+
        *ship-y* +display-half-height+
        *ship-dx* +ship-initial-dx+
        *ship-dy* +ship-initial-dy+
        *ship-direction* +ship-initial-direction+)
  nil)

(defun init-asteroids ()
  (loop :for asteroid :of-type asteroid :across *asteroids* :do
    (setf (usedp asteroid) nil))
  nil)

(defun init-shots ()
  (loop :for shot :of-type shot :across *shots* :do
    (setf (usedp shot) nil))
  nil)

(defun init-game ()
  (init-ship)
  (init-asteroids)
  (init-shots)
  (setf *current-highscore* 0)
  nil)
