(cl:in-package #:pantalea.event-loop)


(defclass event-loop ()
  ((%context
    :initarg :context
    :reader context)
   (%timing-wheel
    :initarg :timing-wheel
    :reader timing-wheel)
   (%queue
    :initarg :queue
    :reader queue))
  (:default-initargs
   :queue (q:make-blocking-queue)))

(defclass event ()
  ((%id
    :initarg :id
    :accessor id))
  (:default-initargs
   :id (random most-positive-fixnum)))
