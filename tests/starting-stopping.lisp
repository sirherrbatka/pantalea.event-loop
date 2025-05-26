(cl:in-package #:pantalea.event-loop)


(let ((event-loop
        (make-instance 'pantalea.event-loop:event-loop)))
  (pantalea.event-loop:start! event-loop)
  (rove:ok (running-p event-loop))
  (pantalea.event-loop:stop! event-loop)
  (rove:ok (not (running-p event-loop))))
