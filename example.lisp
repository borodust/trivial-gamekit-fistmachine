(cl:defpackage :trivial-gamekit.fistmachine.example
  (:use :cl)
  (:export #:run))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defclass loading-screen ()
  ((started-at :initform nil)))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defmethod gamekit.fistmachine:initialize-state ((this loading-screen) &key)
  (with-slots (started-at) this
    (setf started-at (bodge-util:real-time-seconds))))

(defmethod gamekit:act ((this loading-screen))
  (with-slots (started-at) this
    (when (> (- (bodge-util:real-time-seconds) started-at) 3)
      ;; if enough seconds passed since we reached current state
      ;; transition to main-menu
      (gamekit.fistmachine:transition-to 'main-menu))))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defmethod gamekit:draw ((this loading-screen))
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas 300 400)
    (gamekit:scale-canvas 2 2)
    (gamekit:draw-text "LOADING" (gamekit:vec2 0 0))
    (let* ((amplitude 35)
           (x-coord (* amplitude (cos (* (bodge-util:real-time-seconds) 3)))))
      (gamekit:draw-circle (gamekit:vec2 (+ x-coord 29) -10) 2.5
                           :fill-paint (gamekit:vec4 0 0 0 1)))))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defparameter *options* #("START" "QUIT"))

(defclass main-menu ()
  ((selected :initform 0)))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defmethod gamekit.fistmachine:initialize-state ((this main-menu) &key)
  (with-slots (selected) this
    (gamekit:bind-button :down :pressed
                         (lambda ()
                           (setf selected (mod (1+ selected) (length *options*)))))
    (gamekit:bind-button :up :pressed
                         (lambda ()
                           (setf selected (mod (1- selected) (length *options*)))))
    (gamekit:bind-button :enter :pressed
                         (lambda ()
                           (cond
                             ((= selected 0)
                              (gamekit.fistmachine:transition-to 'loading-screen))
                             ((= selected 1)
                              (gamekit:stop)))))))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defmethod gamekit.fistmachine:discard-state ((this main-menu))
  (gamekit:bind-button :down :pressed nil)
  (gamekit:bind-button :up :pressed nil)
  (gamekit:bind-button :enter :pressed nil))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defmethod gamekit:draw ((this main-menu))
  (with-slots (selected) this
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 2 2)
      (loop for text across *options*
            for i from 0
            do (gamekit:draw-text (if (= i selected)
                                      (format nil "~A~4T~A" "=>" text)
                                      (format nil "~4T~A" text))
                                  (gamekit:vec2 140 (+ 130 (- (* i 20)))))))))

(cl:in-package :trivial-gamekit.fistmachine.example)

(gamekit:defgame fistmachine-example (gamekit.fistmachine:fistmachine) ()
  (:viewport-title "FistMachine Example")
  (:default-initargs :initial-state 'loading-screen))

(cl:in-package :trivial-gamekit.fistmachine.example)

(defun run ()
  (gamekit:start 'fistmachine-example))
