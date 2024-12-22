(cl:in-package #:pantalea.event-loop)


(defgeneric timing-wheel (event-loop))
(defgeneric id (event))
(defgeneric add! (event-loop event &optional delay))
(defgeneric react (event loop))
(defgeneric react-with-handler (handler event loop))
(defgeneric obtain-handler (event loop))
(defgeneric obtain-handler-with-id (event loop id))
(defgeneric obtain-handler-without-id (event loop))
