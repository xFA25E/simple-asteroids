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

(defstruct ship-option
  (direction 0d0 :type double-float)
  (dx 0d0 :type double-float)
  (dy 0d0 :type double-float))

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
  (up :released :type keyword)
  (down :released :type keyword)
  (left :released :type keyword)
  (right :released :type keyword)
  (enter :released :type keyword)
  (escape :released :type keyword))

(defclass system (al:system)
  ((ship :type ship :initarg :ship :reader ship)
   (asteroids :type (simple-array asteroid) :initarg :asteroids :reader asteroids)
   (shots :type (simple-array shot) :initarg :shots :reader shots)
   (frames :type (integer 0) :initarg :frames :accessor frames)
   (state :initform :start :type keyword :accessor state)
   (ship-option :type ship-option :initarg :ship-option :reader ship-option)
   (asteroid-options :type (simple-array asteroid-option) :initarg :asteroid-options :reader asteroid-options)
   (shot-option :type shot-option :initarg :shot-option :reader shot-option)
   (controls :type controls :initarg :controls :reader controls)
   (menu :type (simple-array string) :initarg :menu :reader menu)
   (menu-index :type (integer 0 2) :initarg :menu-index :accessor menu-index)
   (menu-font :type foreign-pointer :initarg :menu-font :accessor menu-font)
   (game-over-font :type foreign-pointer :initarg :game-over-font :accessor game-over-font))
  (:default-initargs
   :title "Simple-Asteroids"
   :width 1000 :height 700
   :display-options '((:sample-buffers 1 :suggest) (:samples 8 :suggest))
   :frames 0
   :controls (make-controls)
   :menu #("Game" "Exit" "Highscores")
   :menu-index 0
   :menu-font (null-pointer)
   :game-over-font (null-pointer)

   :ship
   (make-ship
    :thruster-speed 1d0
    :brakes-coefficient 0.96d0
    :direction-speed (* 5 (/ pi 180))
    :nose-length 30
    :wing-length 10)

   :asteroids
   (make-array
    50
    :element-type 'asteroid
    :initial-contents
    (loop :repeat 50 :collect (make-asteroid)))

   :shots
   (make-array
    50
    :element-type 'shot
    :initial-contents
    (loop :repeat 50 :collect (make-shot)))

   :ship-option (make-ship-option)

   :asteroid-options
   (make-array
    3
    :element-type 'asteroid-option
    :initial-contents
    (list (make-asteroid-option :speed 6d0 :radius 15d0 :frequency 60)
          (make-asteroid-option :speed 3d0 :radius 30d0 :frequency 150)
          (make-asteroid-option :speed 1d0 :radius 45d0 :frequency 375)))

   :shot-option
   (make-shot-option :speed 25d0 :radius 2d0 :frequency 5 :duration 90)))
