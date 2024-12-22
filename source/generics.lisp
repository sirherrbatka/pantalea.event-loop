(cl:in-package #:pantalea.event-loop)


(defgeneric timing-wheel (event-loop))
(defgeneric id (event))
(defgeneric add! (event-loop event &optional delay))
(defgeneric react (event event-loop))
(defgeneric react-with-handler (handler event event-loop))
(defgeneric obtain-handler (event event-loop))
(defgeneric obtain-handler-with-id (event loop id))
(defgeneric obtain-handler-without-id (event event-loop))
(defgeneric start! (event-loop))
(defgeneric stop! (event-loop))
