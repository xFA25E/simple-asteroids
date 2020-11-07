(in-package :simple-asteroids)

(declaim
 (inline collide-ship-asteroid-p collide-shot-asteroid-p)
 (ftype (function (asteroid) (values boolean &optional)) collide-ship-asteroid-p)
 (ftype (function (shot asteroid) (values boolean &optional)) collide-shot-asteroid-p))

(defun collide-ship-asteroid-p (asteroid)
  (let ((radius-squared (radius-squared asteroid))
        (x (x asteroid)) (y (y asteroid))
        (c1x 0d0) (c1y 0d0) (c2x 0d0) (c2y 0d0) (c3x 0d0) (c3y 0d0)
        (e1x 0d0) (e1y 0d0) (e2x 0d0) (e2y 0d0) (e3x 0d0) (e3y 0d0)
        (c1-squared 0d0) (c2-squared 0d0) (c3-squared 0d0))
    (declare
     (dynamic-extent c1x c1y c2x c2y c3x c3y e1x e1y e2x e2y e3x e3y c1-squared c2-squared c3-squared)
     (type double-float c1x c1y c2x c2y c3x c3y e1x e1y e2x e2y e3x e3y c1-squared c2-squared c3-squared)
     (type double-float radius-squared x y))

    (setf c1x (- x *ship-nose-x*) c1y (- y *ship-nose-y*)
          c1-squared (- (+ (expt c1x 2) (expt c1y 2)) radius-squared))
    (when (<= c1-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf c2x (- x *ship-right-wing-x*) c2y (- y *ship-right-wing-y*)
          c2-squared (- (+ (expt c2x 2) (expt c2y 2)) radius-squared))
    (when (<= c2-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf c3x (- x *ship-left-wing-x*) c3y (- y *ship-left-wing-y*)
          c3-squared (- (+ (expt c3x 2) (expt c3y 2)) radius-squared))
    (when (<= c3-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf e1x (- *ship-right-wing-x* *ship-nose-x*) e1y (- *ship-right-wing-y* *ship-nose-y*)
          e2x (- *ship-left-wing-x* *ship-right-wing-x*) e2y (- *ship-left-wing-y* *ship-right-wing-y*)
          e3x (- *ship-nose-x* *ship-left-wing-x*) e3y (- *ship-nose-y* *ship-left-wing-y*))

    (when (and (<= (* e1y c1x) (* e1x c1y))
               (<= (* e2y c2x) (* e2x c2y))
               (<= (* e3y c3x) (* e3x c3y)))
      (return-from collide-ship-asteroid-p t))

    (let ((k (+ (* c1x e1x) (* c1y e1y))))
      (declare (type double-float k))
      (when (< 0 k)
        (let ((len (+ (expt e1x 2) (expt e1y 2))))
          (declare (type double-float len))
          (when (and (< k len) (<= (* c1-squared len) (expt k 2)))
            (return-from collide-ship-asteroid-p t)))))

    (let ((k (+ (* c2x e2x) (* c2y e2y))))
      (declare (type double-float k))
      (when (< 0 k)
        (let ((len (+ (expt e2x 2) (expt e2y 2))))
          (declare (type double-float len))
          (when (and (< k len) (<= (* c2-squared len) (expt k 2)))
            (return-from collide-ship-asteroid-p t)))))

    (let ((k (+ (* c3x e3x) (* c3y e3y))))
      (declare (type double-float k))
      (when (< 0 k)
        (let ((len (+ (expt e3x 2) (expt e3y 2))))
          (declare (type double-float len))
          (when (and (< k len) (<= (* c3-squared len) (expt k 2)))
            (return-from collide-ship-asteroid-p t)))))

    nil))

(defun collide-shot-asteroid-p (shot asteroid)
  (<= (+ (expt (- (x asteroid) (x shot)) 2)
         (expt (- (y asteroid) (y shot)) 2))
      (expt (+ (radius asteroid) (radius shot)) 2)))
