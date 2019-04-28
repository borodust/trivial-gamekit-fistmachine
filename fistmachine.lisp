(cl:defpackage :trivial-gamekit.fistmachine
  (:nicknames :gamekit.fistmachine)
  (:use :cl)
  (:export #:fistmachine
           #:initialize-state
           #:discard-state
           #:transition-to))
(cl:in-package :trivial-gamekit.fistmachine)


(defclass fistmachine ()
  ((game-state :initform nil)
   (initial-state :initarg :initial-state :initform nil)))


(defgeneric initialize-state (game-state &key &allow-other-keys)
  (:method (game-state &key &allow-other-keys)
    (declare (ignore game-state))))


(defgeneric discard-state (game-state)
  (:method (game-state) (declare (ignore game-state))))


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
                          (discard-state game-state)
                          (setf game-state (apply #'make-instance state-class args))
                          (apply #'initialize-state game-state args))
                      (retry-transition ()
                        :report %report-retry
                        (go begin))))))
        (gamekit:push-action #'%transition-to)))))


(defgeneric button-pressed (game-state button)
  (:method (game-state button) (declare (ignore game-state button))))


(defgeneric button-released (game-state button)
  (:method (game-state button) (declare (ignore game-state button))))


(defmethod gamekit:post-initialize ((this fistmachine))
  (call-next-method)
  (with-slots (game-state initial-state) this
    (flet ((process-button (button state)
             (case state
               (:pressed (button-pressed game-state button))
               (:released (button-released game-state button)))))
      (gamekit:bind-any-button #'process-button))
    (when initial-state
      (transition-to initial-state))))


(defmethod gamekit:act ((this fistmachine))
  (call-next-method)
  (with-slots (game-state) this
    (gamekit:act game-state)))


(defmethod gamekit:draw ((this fistmachine))
  (call-next-method)
  (with-slots (game-state) this
    (gamekit:draw game-state)))
