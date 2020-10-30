(in-package :simple-asteroids)

(defmethod al:system-loop :around ((sys system))
  (with-font (game-over
              (al:load-ttf-font
               "/usr/share/fonts/TTF/DejaVuSansMono-Bold.ttf" 46 0))
    (with-slots (game-over-font) sys
      (setf game-over-font game-over))
    (call-next-method)))

(defmethod al:key-down-handler ((sys system))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (setf (upp (controls sys)) t))
    (:down (setf (downp (controls sys)) t))
    (:left (setf (leftp (controls sys)) t))
    (:right (setf (rightp (controls sys)) t))
    (:escape (setf (escapep (controls sys)) t))))

(defmethod al:key-up-handler ((sys system))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (setf (upp (controls sys)) nil))
    (:down (setf (downp (controls sys)) nil))
    (:left (setf (leftp (controls sys)) nil))
    (:right (setf (rightp (controls sys)) nil))
    (:escape (setf (escapep (controls sys)) nil))))

(defmethod al:update ((sys system))
  (ecase (state sys)
    (:start (setf (state sys) :game))
    (:game
     (let ((controls (controls sys)))
       (if (escapep controls)
           (setf (state sys) :end)
           (let ((ship (ship sys))
                 (shots (shots sys))
                 (asteroids (asteroids sys))
                 (frames (frames sys))
                 (width (al:width sys))
                 (height (al:height sys))
                 (shot-option (shot-option sys))
                 (asteroid-options (asteroid-options sys)))

             (update-ship ship width height controls)
             (update-shots shots width height)
             (update-asteroids asteroids width height)

             (when (zerop (mod frames (shot-option-frequency shot-option)))
               (add-shot shots ship shot-option))

             (loop :for asteroid-option :across asteroid-options
                   :when (zerop (mod frames (asteroid-option-frequency asteroid-option))) :do
                     (add-asteroid asteroids width height asteroid-option))

             (loop :for asteroid :across asteroids
                   :when (usedp asteroid) :do
                     (loop :for shot :across shots
                           :when (and (usedp shot) (collide-shot-asteroid-p shot asteroid)) :do
                             (setf (usedp shot) nil
                                   (usedp asteroid) nil)))

             (loop :for asteroid :across asteroids
                   :when (and (usedp asteroid) (collide-ship-asteroid-p ship asteroid))
                     :do (setf (state sys) :game-over)
                     :and :do (loop-finish))

             (incf (frames sys))))))
    (:game-over
     (when (escapep (controls sys))
       (setf (state sys) :end)))
    (:end (setf (al:system-loop-running-p sys) nil))))

(defmethod al:render ((sys system))
  (ecase (state sys)
    (:start)
    (:game
     (al:clear-to-color *black-color*)
     (render-ship (ship sys))
     (render-shots (shots sys))
     (render-asteroids (asteroids sys))
     (al:flip-display))
    (:game-over
     (al:clear-to-color *black-color*)
     (al:draw-text (game-over-font sys) *white-color*
                   (/ (al:width sys) 2) (/ (al:height sys) 2)
                   (foreign-enum-value 'al::align-flags :center)
                   "G A M E  O V E R")
     (al:flip-display))
    (:end)))

(defun main ()
  (let* ((width 800)
         (height 600)
         (max-asteroids 50)
         (max-shots 50)
         (system
           (make-instance
            'system
            :title "Simple-Asteroids"
            :width width :height height
            :display-options '((:sample-buffers 1 :suggest) (:samples 8 :suggest))
            :ship (make-ship :x (/ width 2d0) :y (/ height 2d0)
                             :thruster-speed 1d0 :brakes-coefficient 0.96d0
                             :direction-speed (* 5 (/ pi 180))
                             :nose-length 30 :wing-length 10)
            :asteroids (make-array max-asteroids
                                   :element-type 'asteroid
                                   :initial-contents (loop :repeat max-asteroids :collect (make-asteroid)))
            :shots (make-array max-shots
                               :element-type 'shot
                               :initial-contents (loop :repeat max-shots :collect (make-shot)))
            :asteroid-options
            (make-array 3 :element-type 'asteroid-option
                          :initial-contents
                          (list (make-asteroid-option :speed 6d0 :radius 15d0 :frequency 60)
                                (make-asteroid-option :speed 3d0 :radius 30d0 :frequency 150)
                                (make-asteroid-option :speed 1d0 :radius 45d0 :frequency 375)))
            :shot-option (make-shot-option :speed 25d0 :radius 2d0 :frequency 5 :duration 90))))
    (al:run-system system)))
