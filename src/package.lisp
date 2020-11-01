(defpackage simple-asteroids
  (:use :cl)
  (:import-from #:alexandria #:random-elt)
  (:import-from #:cffi
                #:foreign-slot-value
                #:null-pointer-p
                #:null-pointer
                #:foreign-pointer
                #:foreign-enum-value))

(declaim (inline init-ship init-asteroids init-shots init-game
                 update-asteroids update-shots update-ship update-controls
                 update-game update-menu update-game-over update-highscores
                 add-shot add-asteroid
                 render-ship render-asteroids render-shots
                 render-game render-menu render-game-over render-highscores
                 collide-ship-asteroid-p collide-shot-asteroid-p))
