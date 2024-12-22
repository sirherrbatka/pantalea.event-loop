(cl:in-package #:pantalea.event-loop)


(defclass event-loop ()
  ((%context
    :initarg :context
    :accessor context)
   (%timing-wheel
    :initarg :timing-wheel
    :accessor timing-wheel)
   (%main-lock
    :initarg :main-lock
    :reader main-lock)
   (%thread
    :initarg :thread
    :accessor thread)
   (%queue
    :initarg :queue
    :accessor queue))
  (:default-initargs
   :thread nil
   :context nil
   :timing-wheel nil
   :main-lock (bt2:make-lock)
   :queue (q:make-blocking-queue)))

(defclass event ()
  ((%id
    :initarg :id
    :accessor id))
  (:default-initargs
   :id (random most-positive-fixnum)))

(defclass termination-event ()
  ())

(define-condition termination-condition ()
  ())
