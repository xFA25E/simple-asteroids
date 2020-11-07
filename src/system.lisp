(in-package :simple-asteroids)

(declaim
 (type display-size
       +display-width+ +display-height+
       +asteroid-bitmap-width+ +asteroid-bitmap-height+
       +ship-bitmap-width+ +ship-bitmap-height+
       +fire-bitmap-width+ +fire-bitmap-height+
       +explosion-1-bitmap-width+ +explosion-1-bitmap-height+
       +explosion-2-bitmap-width+ +explosion-2-bitmap-height+
       +star-bitmap-width+ +star-bitmap-height+)
 (type display-float
       +display-half-width+ +display-half-height+
       +display-width-d0+ +display-height-d0+
       +asteroid-bitmap-half-width+ +asteroid-bitmap-half-height+
       +ship-bitmap-half-width+ +ship-bitmap-half-height+
       +fire-bitmap-half-width+ +fire-bitmap-half-height+
       +explosion-1-bitmap-half-width+ +explosion-1-bitmap-half-height+
       +explosion-2-bitmap-half-width+ +explosion-2-bitmap-half-height+
       +fire-bitmap-distance+
       +menu-font-start+ +menu-font-block+ +menu-font-size+
       +game-over-font-start+ +game-over-font-block+ +game-over-font-size+
       +highscores-font-start+ +highscores-font-block+ +highscores-font-size+
       +current-highscore-font-size+
       +ship-thruster-power+ +ship-brakes-coefficient+
       +ship-half-nose-length+ +ship-wing-length+
       +shot-speed+ +shot-radius+
       +star-speed+
       *ship-x* *ship-y*
       *ship-nose-x* *ship-nose-y*
       *ship-right-wing-x* *ship-right-wing-y*
       *ship-left-wing-x* *ship-left-wing-y*
       +ship-initial-dx+ +ship-initial-dy+ *ship-dx* *ship-dy*
       +asteroid-bitmap-half-width-reciprocal+
       +asteroid-bitmap-half-height-reciprocal+)
 (type radiants
       +ship-initial-direction+
       +ship-rotation-speed+
       +asteroid-rotation-speed+
       +star-rotation-speed+
       *ship-direction*)
 (type frequency +shot-frequency+)
 (type duration
       +shot-duration+
       +explosion-stage-1-start+ +explosion-stage-2-start+
       *frames*)
 (type highscore *current-highscore*)
 (type resource-size +stars-count+ +asteroids-count+ +shots-count+ +explosions-count+)
 (type string
       *font-asset*
       *background-bitmap-asset* *sheet-bitmap-asset*
       *explosion-1-audio-asset* *explosion-2-audio-asset*)
 (type foreign-pointer
       *background-bitmap*
       *ship-bitmap*
       *fire-bitmap*
       *star-bitmap*
       *asteroid-bitmap*
       *explosion-1-bitmap* *explosion-2-bitmap*
       *menu-font* *game-over-font* *highscores-font* *current-highscore-font*
       *explosion-1-sample* *explosion-2-sample*)
 (type key-state *key-up* *key-down* *key-left* *key-right* *key-enter* *key-escape*)
 (type (simple-array asteroid-initials (3)) *asteroid-initials*)
 (type (resources asteroid) *asteroids*)
 (type (resources shot) *shots*)
 (type (resources star) *stars*)
 (type (resources explosion) *explosions*)
 (type game-state *state*)
 (type menu *menu*)
 (type menu-index *menu-index*)
 (type highscores *highscores*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CONSTANTS                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RENDERING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DISPLAY

(defconstant +display-width+ 960)
(defconstant +display-height+ 640)

(defconstant +display-half-width+ (/ +display-width+ 2d0))
(defconstant +display-half-height+ (/ +display-height+ 2d0))

(defconstant +display-width-d0+ (coerce +display-width+ 'display-float))
(defconstant +display-height-d0+ (coerce +display-height+ 'display-float))

;;; BITMAPS

(defconstant +asteroid-bitmap-width+ 50)
(defconstant +asteroid-bitmap-height+ 50)
(defconstant +asteroid-bitmap-half-width+ (/ +asteroid-bitmap-width+ 2d0))
(defconstant +asteroid-bitmap-half-height+ (/ +asteroid-bitmap-height+ 2d0))
(defconstant +asteroid-bitmap-half-width-reciprocal+ (/ 1d0 +asteroid-bitmap-half-width+))
(defconstant +asteroid-bitmap-half-height-reciprocal+ (/ 1d0 +asteroid-bitmap-half-height+))

(defconstant +ship-bitmap-width+ 63)
(defconstant +ship-bitmap-height+ 42)
(defconstant +ship-bitmap-half-width+ (/ +ship-bitmap-width+ 2d0))
(defconstant +ship-bitmap-half-height+ (/ +ship-bitmap-height+ 2d0))

(defconstant +fire-bitmap-width+ 28)
(defconstant +fire-bitmap-height+ 27)
(defconstant +fire-bitmap-half-width+ (/ +fire-bitmap-width+ 2d0))
(defconstant +fire-bitmap-half-height+ (/ +fire-bitmap-height+ 2d0))

(defconstant +explosion-1-bitmap-width+ 59)
(defconstant +explosion-1-bitmap-height+ 60)
(defconstant +explosion-1-bitmap-half-width+ (/ +explosion-1-bitmap-width+ 2d0))
(defconstant +explosion-1-bitmap-half-height+ (/ +explosion-1-bitmap-height+ 2d0))

(defconstant +explosion-2-bitmap-width+ 59)
(defconstant +explosion-2-bitmap-height+ 59)
(defconstant +explosion-2-bitmap-half-width+ (/ +explosion-2-bitmap-width+ 2d0))
(defconstant +explosion-2-bitmap-half-height+ (/ +explosion-2-bitmap-height+ 2d0))

(defconstant +star-bitmap-width+ 7)
(defconstant +star-bitmap-height+ 7)

(defconstant +fire-bitmap-distance+ (+ +ship-bitmap-half-height+ +fire-bitmap-half-height+))

;;; FONTS

(defconstant +menu-font-start+ (* 0.25d0 +display-height-d0+))
(defconstant +menu-font-block+ (/ (* 0.5d0 +display-height-d0+) 3d0))
(defconstant +menu-font-size+ (round (* 0.7d0 +menu-font-block+)))

(defconstant +game-over-font-start+ (* 0.25d0 +display-height-d0+))
(defconstant +game-over-font-block+ (/ (* 0.5d0 +display-height-d0+) 2d0))
(defconstant +game-over-font-size+ (round (* 0.7d0 +game-over-font-block+)))

(defconstant +highscores-font-start+ (* 0.10d0 +display-height-d0+))
(defconstant +highscores-font-block+ (/ (* 0.8d0 +display-height-d0+) 5d0))
(defconstant +highscores-font-size+ (round (* 0.8d0 +highscores-font-block+)))

(defconstant +current-highscore-font-size+ (round (* 0.1d0 +display-height-d0+)))

;; GAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SHIP

(defconstant +ship-initial-direction+ 0d0)
(defconstant +ship-initial-dx+ 0d0)
(defconstant +ship-initial-dy+ 0d0)

(defconstant +ship-thruster-power+ 1d0)
(defconstant +ship-brakes-coefficient+ 0.96d0)
(defconstant +ship-rotation-speed+ (* 5d0 (/ pi 180d0)))
(defconstant +ship-half-nose-length+ (coerce +ship-bitmap-half-width+ 'display-float))
(defconstant +ship-wing-length+ +ship-bitmap-half-height+)

;;; SHOT

(defconstant +shot-speed+ 25d0)
(defconstant +shot-radius+ 3d0)
(defconstant +shot-frequency+ 3)
(defconstant +shot-duration+ 45)

;;; ASTERIOD

(defconstant +asteroid-rotation-speed+ (* 2d0 (/ pi 180d0)))

;;; STAR

(defconstant +star-rotation-speed+ (* 2d0 (/ pi 180d0)))
(defconstant +star-speed+ 0.25d0)

;;; RESOURCES COUNTS

(defconstant +stars-count+ 50)
(defconstant +asteroids-count+ 50)
(defconstant +shots-count+ 50)
(defconstant +explosions-count+ 50)

;;; EXPOLSION STAGES

(defconstant +explosion-stage-1-start+ 10)
(defconstant +explosion-stage-2-start+ 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 VARIABLES                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RENDERING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ASSETS

(defvar *font-asset* (asset "fonts/simplicity.ttf"))
(defvar *background-bitmap-asset* (asset "images/background.png"))
(defvar *sheet-bitmap-asset* (asset "images/sheet.png"))
(defvar *explosion-1-audio-asset* (asset "audio/explosion1.flac"))
(defvar *explosion-2-audio-asset* (asset "audio/explosion2.flac"))

;;; BITMAPS

(defvar *background-bitmap* (null-pointer))
(defvar *ship-bitmap* (null-pointer))
(defvar *fire-bitmap* (null-pointer))
(defvar *star-bitmap* (null-pointer))
(defvar *asteroid-bitmap* (null-pointer))
(defvar *explosion-1-bitmap* (null-pointer))
(defvar *explosion-2-bitmap* (null-pointer))

;;; FONTS

(defvar *menu-font* (null-pointer))
(defvar *game-over-font* (null-pointer))
(defvar *highscores-font* (null-pointer))
(defvar *current-highscore-font* (null-pointer))

;;; COLORS

(defvar *main-color* (al:map-rgb-f 0.344 0.188 0.509))

;;; SAMPLES

(defvar *explosion-1-sample* (null-pointer))
(defvar *explosion-2-sample* (null-pointer))

;; GAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONTROLS

(defvar *key-up* :released)
(defvar *key-down* :released)
(defvar *key-left* :released)
(defvar *key-right* :released)
(defvar *key-enter* :released)
(defvar *key-escape* :released)

;;; SHIP

(defvar *ship-x* 0d0)
(defvar *ship-y* 0d0)
(defvar *ship-dx* 0d0)
(defvar *ship-dy* 0d0)
(defvar *ship-direction* 0d0)
(defvar *ship-nose-x* 0d0)
(defvar *ship-nose-y* 0d0)
(defvar *ship-right-wing-x* 0d0)
(defvar *ship-right-wing-y* 0d0)
(defvar *ship-left-wing-x* 0d0)
(defvar *ship-left-wing-y* 0d0)

;;; ASTEROID

(defvar *asteroid-initials*
  `#(,(make-asteroid-initials :speed 6d0 :radius 15d0 :frequency 60)
     ,(make-asteroid-initials :speed 3d0 :radius 30d0 :frequency 150)
     ,(make-asteroid-initials :speed 1d0 :radius 45d0 :frequency 375)))

;;; RESOURCES

(defvar *asteroids*
  (loop :with asteroids :of-type (resources asteroid)
          = (make-array +asteroids-count+
                        :element-type 'asteroid
                        :initial-element (make-asteroid))
        :for i fixnum :from 0 :below +asteroids-count+ :do
          (setf (aref asteroids i) (make-asteroid))
        :finally (return asteroids)))

(defvar *shots*
  (loop :with shots :of-type (resources shot)
          = (make-array +shots-count+
                        :element-type 'shot
                        :initial-element (make-shot))
        :for i fixnum :from 0 :below +shots-count+ :do
          (setf (aref shots i) (make-shot))
        :finally (return shots)))

(defvar *stars*
  (loop :with stars :of-type (resources star)
          = (make-array +stars-count+
                        :element-type 'star
                        :initial-element (make-star))
        :for i fixnum :from 0 :below +stars-count+ :do
          (setf (aref stars i)
                (let ((direction (random +two-pi+)))
                  (declare (type radiants direction))
                  (make-star
                   :x (random +display-width-d0+)
                   :y (random +display-height-d0+)
                   :dx (* +star-speed+ (cos direction))
                   :dy (* +star-speed+ (sin direction))
                   :star-direction direction)))
        :finally (return stars)))

(defvar *explosions*
  (loop :with explosions :of-type (resources explosion)
          = (make-array +explosions-count+
                        :element-type 'explosion
                        :initial-element (make-explosion))
        :for i fixnum :from 0 :below +explosions-count+ :do
          (setf (aref explosions i) (make-explosion))
        :finally (return explosions)))

;;; OTHER

(defvar *frames* 0)
(defvar *state* :start)
(defvar *menu* #("Game" "Highscores" "Exit"))
(defvar *menu-index* 0)
(defvar *current-highscore* 0)
(defvar *highscores* (make-array 5 :element-type 'highscore))

(defclass system (al:system)
  ()
  (:default-initargs
   :title "Simple-Asteroids"
   :width +display-width+ :height +display-height+
   :display-options '((:sample-buffers 1 :suggest) (:samples 8 :suggest))))
