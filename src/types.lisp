(in-package :simple-asteroids)

(defstruct (point (:conc-name))
  (x 0d0 :type double-float)
  (y 0d0 :type double-float))

(defstruct (moving-point (:include point) (:conc-name))
  (dx 0d0 :type double-float)
  (dy 0d0 :type double-float))

(defstruct (moving-circle (:include moving-point) (:conc-name))
  (radius 0d0 :type double-float))

(defstruct (moving-circular-resource (:include moving-circle) (:conc-name))
  (usedp nil :type boolean))

(defstruct (asteroid (:include moving-circular-resource) (:conc-name))
  (radius-squared 0d0 :type double-float))

(defstruct (shot (:include moving-circular-resource) (:conc-name))
  (duration 0 :type (integer 0)))

(defstruct (ship (:include moving-point) (:conc-name))
  (thruster-speed 0d0 :type double-float)
  (brakes-coefficient 0d0 :type double-float)
  (direction 0d0 :type double-float)
  (direction-speed 0d0 :type double-float)
  (nose-length 0 :type (integer 0 4000))
  (wing-length 0 :type (integer 0 4000))
  (nose (make-point) :type point)
  (right-wing (make-point) :type point)
  (left-wing (make-point) :type point))

(defstruct asteroid-option
  (speed 0d0 :type double-float)
  (radius 0d0 :type double-float)
  (frequency 1 :type (integer 1)))

(defstruct shot-option
  (speed 0d0 :type double-float)
  (radius 0d0 :type double-float)
  (frequency 1 :type (integer 1))
  (duration 0 :type (integer 0)))

(defstruct (controls (:conc-name))
  (upp nil :type boolean)
  (downp nil :type boolean)
  (leftp nil :type boolean)
  (rightp nil :type boolean)
  (escapep nil :type boolean))

(defclass system (al:system)
  ((ship
    :initform (make-ship)
    :type ship
    :initarg :ship
    :reader ship)
   (asteroids
    :initform (make-array 0)
    :type (simple-array asteroid)
    :initarg :asteroids
    :reader asteroids)
   (shots
    :initform (make-array 0)
    :type (simple-array shot)
    :initarg :shots
    :reader shots)
   (frames :initform 0 :type (integer 0) :accessor frames)
   (state :initform :start :type keyword :accessor state)
   (asteroid-options
    :initform (make-array 0)
    :type (simple-array asteroid-option)
    :initarg :asteroid-options
    :reader asteroid-options)
   (shot-option
    :initform (make-shot-option)
    :type shot-option
    :initarg :shot-option
    :reader shot-option)
   (controls :initform (make-controls) :type controls :reader controls)
   (game-over-font :initform (null-pointer) :type foreign-pointer :accessor game-over-font)))
