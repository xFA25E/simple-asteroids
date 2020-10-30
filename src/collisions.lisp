(in-package :simple-asteroids)

(defun collide-ship-asteroid-p (ship asteroid)
  (declare (ship ship) (asteroid asteroid))

  (let* ((radius-squared (radius-squared asteroid))
         (x (x asteroid))
         (y (y asteroid))
         (nose (nose ship))
         (right-wing (right-wing ship))
         (left-wing (left-wing ship))
         (v1x (x nose)) (v1y (y nose))
         (v2x (x right-wing)) (v2y (y right-wing))
         (v3x (x left-wing)) (v3y (y left-wing))
         c1x c1y c2x c2y c3x c3y
         e1x e1y e2x e2y e3x e3y
         c1-squared c2-squared c3-squared)
    (setf c1x (- x v1x) c1y (- y v1y)
          c1-squared (- (+ (expt c1x 2) (expt c1y 2)) radius-squared))
    (when (<= c1-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf c2x (- x v2x) c2y (- y v2y)
          c2-squared (- (+ (expt c2x 2) (expt c2y 2)) radius-squared))
    (when (<= c2-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf c3x (- x v3x) c3y (- y v3y)
          c3-squared (- (+ (expt c3x 2) (expt c3y 2)) radius-squared))
    (when (<= c3-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf e1x (- v2x v1x) e1y (- v2y v1y)
          e2x (- v3x v2x) e2y (- v3y v2y)
          e3x (- v1x v3x) e3y (- v1y v3y))

    (when (and (<= (* e1y c1x) (* e1x c1y))
               (<= (* e2y c2x) (* e2x c2y))
               (<= (* e3y c3x) (* e3x c3y)))
      (return-from collide-ship-asteroid-p t))

    (let ((k (+ (* c1x e1x) (* c1y e1y))))
      (when (< 0 k)
        (let* ((len (+ (expt e1x 2) (expt e1y 2))))
          (when (< k len)
            (when (<= (* c1-squared len) (expt k 2))
              (return-from collide-ship-asteroid-p t))))))

    (let ((k (+ (* c2x e2x) (* c2y e2y))))
      (when (< 0 k)
        (let* ((len (+ (expt e2x 2) (expt e2y 2))))
          (when (< k len)
            (when (<= (* c2-squared len) (expt k 2))
              (return-from collide-ship-asteroid-p t))))))

    (let ((k (+ (* c3x e3x) (* c3y e3y))))
      (when (< 0 k)
        (let* ((len (+ (expt e3x 2) (expt e3y 2))))
          (when (< k len)
            (when (<= (* c3-squared len) (expt k 2))
              (return-from collide-ship-asteroid-p t))))))

    nil))

(defun collide-shot-asteroid-p (shot asteroid)
  (declare (shot shot) (asteroid asteroid))
  (<= (+ (expt (- (x asteroid) (x shot)) 2)
         (expt (- (y asteroid) (y shot)) 2))
      (expt (+ (radius asteroid) (radius shot)) 2)))
