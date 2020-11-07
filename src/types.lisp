(in-package :simple-asteroids)

(deftype display-size () '(integer 0 4000))
(deftype display-float () '(double-float -4000d0 4000d0))
(deftype radiants () `(double-float 0d0 ,(* 2 (coerce pi 'double-float))))
(deftype frequency () `(integer 1 ,most-positive-fixnum))
(deftype duration () `(integer 0 ,most-positive-fixnum))
(deftype resource-size () '(integer 50 50))
(deftype resources (kind) `(simple-array ,kind (50)))
(deftype key-state () '(member :pressed :keep :released))
(deftype game-state () '(member :game :menu :game-over :highscores :start :end))
(deftype highscore () `(integer 0 ,most-positive-fixnum))
(deftype highscores () '(simple-array highscore (5)))
(deftype menu () '(simple-array string (3)))
(deftype menu-index () '(mod 3))

(defstruct (point (:conc-name))
  (x 0d0 :type display-float)
  (y 0d0 :type display-float))

(defstruct (moving-point (:include point) (:conc-name))
  (dx 0d0 :type display-float)
  (dy 0d0 :type display-float))

(defstruct (moving-point-resource (:include moving-point) (:conc-name))
  (usedp nil :type boolean))

(defstruct (moving-circular-resource (:include moving-point-resource) (:conc-name))
  (radius 0d0 :type display-float))

(defstruct (asteroid (:include moving-circular-resource) (:conc-name))
  (radius-squared 0d0 :type display-float)
  (asteroid-direction 0d0 :type radiants)
  (x-scale 0d0 :type display-float)
  (y-scale 0d0 :type display-float))

(defstruct (shot (:include moving-circular-resource) (:conc-name))
  (duration 0 :type duration))

(defstruct (star (:include moving-point) (:conc-name))
  (star-direction 0d0 :type radiants))

(defstruct (explosion (:include point) (:conc-name))
  (explosion-usedp nil :type boolean)
  (explosion-duration 0 :type duration))

(defstruct (asteroid-initials (:conc-name asteroid-))
  (speed 0d0 :type display-float)
  (radius 0d0 :type display-float)
  (frequency 1 :type frequency))
