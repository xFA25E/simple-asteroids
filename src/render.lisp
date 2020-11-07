(in-package :simple-asteroids)

(declaim
 (inline render-explosions render-stars
         render-ship render-asteroids
         render-shots render-game render-menu
         render-game-over render-highscores)
 (ftype (function nil (values null &optional))
        render-explosions render-stars
        render-ship render-asteroids
        render-shots render-game render-menu
        render-game-over render-highscores))

(defun render-explosions ()
  (loop :for explosion :of-type explosion :across *explosions*
        :when (explosion-usedp explosion) :do
          (cond ((<= 0 (explosion-duration explosion) +explosion-stage-2-start+)
                 (al:draw-bitmap
                  *explosion-2-bitmap*
                  (- (x explosion) +explosion-2-bitmap-half-width+)
                  (- (y explosion) +explosion-2-bitmap-half-height+)
                  nil))
                ((<= +explosion-stage-2-start+ (explosion-duration explosion) +explosion-stage-1-start+)
                 (al:draw-bitmap
                  *explosion-1-bitmap*
                  (- (x explosion) +explosion-1-bitmap-half-width+)
                  (- (y explosion) +explosion-1-bitmap-half-height+)
                  nil))))
  nil)

(defun render-stars ()
  (loop :for star :of-type star :across *stars* :do
    (al:draw-rotated-bitmap
     *star-bitmap*
     0 0 (x star) (y star)
     (star-direction star)
     nil))
  nil)

(defun render-ship ()
  (al:draw-rotated-bitmap
   *ship-bitmap*
   +ship-bitmap-half-width+ +ship-bitmap-half-height+
   *ship-x* *ship-y*
   *ship-direction*
   nil)

  (unless (and (eq :released *key-up*) (eq :released *key-down*)
               (eq :released *key-left*) (eq :released *key-right*))
    (al:draw-rotated-bitmap
     *fire-bitmap*
     +fire-bitmap-half-width+ +fire-bitmap-half-height+
     (- *ship-x* (* +fire-bitmap-distance+ (cos *ship-direction*)))
     (- *ship-y* (* +fire-bitmap-distance+ (sin *ship-direction*)))
     *ship-direction*
     nil))
  nil)

(defun render-asteroids ()
  (loop :for asteroid :of-type asteroid :across *asteroids*
        :when (usedp asteroid) :do
          (al:draw-scaled-rotated-bitmap
           *asteroid-bitmap*
           +asteroid-bitmap-half-width+
           +asteroid-bitmap-half-height+
           (x asteroid) (y asteroid)
           (x-scale asteroid)
           (y-scale asteroid)
           (asteroid-direction asteroid)
           nil))
  nil)

(defun render-shots ()
  (loop :for shot :of-type shot :across *shots*
        :when (usedp shot) :do
          (al:draw-filled-circle
           (x shot) (y shot) (radius shot)
           *main-color*))
  nil)

(defun render-game ()
  (al:draw-text *current-highscore-font* *main-color* 1 1
                (foreign-enum-value 'al::align-flags :left)
                (write-to-string *current-highscore*))
  (render-ship)
  (render-shots)
  nil)

(defun render-menu ()
  (loop :for entry :of-type string :across *menu*
        :for i :of-type menu-index :from 0
        :for y :of-type display-float = +menu-font-start+ :then (+ y +menu-font-block+)
        :do (al:draw-text *menu-font* *main-color*
                          +display-half-width+ y
                          (foreign-enum-value 'al::align-flags :center)
                          (if (= i *menu-index*)
                              (concatenate 'string "< " entry " >")
                              entry))))

(defun render-game-over ()
  (al:draw-text *game-over-font* *main-color*
                +display-half-width+ +game-over-font-start+
                (foreign-enum-value 'al::align-flags :center)
                "G A M E  O V E R")
  (al:draw-text *game-over-font* *main-color*
                +display-half-width+ (+ +game-over-font-start+ +game-over-font-block+)
                (foreign-enum-value 'al::align-flags :center)
                (concatenate 'string "Score: " (write-to-string *current-highscore*))))

(defun render-highscores ()
  (loop :for highscore :of-type highscore :across *highscores*
        :for y :of-type display-float = +highscores-font-start+ :then (+ y +highscores-font-block+) :do
          (al:draw-text *highscores-font* *main-color*
                        +display-half-width+ y
                        (foreign-enum-value 'al::align-flags :center)
                        (write-to-string highscore))))
