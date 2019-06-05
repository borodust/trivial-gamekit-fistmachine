(cl:defpackage :trivial-gamekit.fistmachine
  (:nicknames :gamekit.fistmachine)
  (:use :cl)
  (:export #:fistmachine
           #:transition-to
           #:current-state))
(cl:in-package :trivial-gamekit.fistmachine)


(defclass fistmachine ()
  ((game-state :initform nil)
   (initial-state :initarg :initial-state :initform nil)))


(defun transition-to (state-class &rest args &key &allow-other-keys)
  (let ((game (gamekit:gamekit)))
    (with-slots (game-state) game
      (labels ((%report-retry (stream)
                 (format stream "Try transitioning to ~A again" state-class))
               (%transition-to ()
                 (tagbody
                  begin
                    (restart-case
                        (progn
                          (gamekit:pre-destroy game-state)
                          (setf game-state (apply #'make-instance state-class args))
                          (gamekit:post-initialize game-state))
                      (retry-transition ()
                        :report %report-retry
                        (go begin))))))
        (gamekit:push-action #'%transition-to)))))


(defmethod gamekit:post-initialize :after ((this fistmachine))
  (with-slots (game-state initial-state) this
    (when initial-state
      (transition-to initial-state))))


(defmethod gamekit:pre-destroy :before ((this fistmachine))
  (with-slots (game-state) this
    (when game-state
      (gamekit:pre-destroy game-state))))


(defmethod gamekit:act :after ((this fistmachine))
  (with-slots (game-state) this
    (gamekit:act game-state)))


(defmethod gamekit:draw :after ((this fistmachine))
  (with-slots (game-state) this
    (gamekit:draw game-state)))


(defmethod gamekit:notice-resources :after ((this fistmachine) &rest args)
  (with-slots (game-state) this
    (apply #'gamekit:notice-resources game-state args)))


(defun current-state ()
  (slot-value (gamekit:gamekit) 'game-state))
