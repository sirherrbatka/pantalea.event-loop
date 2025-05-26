(cl:in-package #:pantalea.event-loop)


(let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
  (pantalea.event-loop:start! event-loop)
  (rove:ok (running-p event-loop))
  (pantalea.event-loop:events-sequence
      ((a (:delay 3)
          5)
       (b (:success (a) :delay 5)
          (+ 2 a)))
    (pantalea.event-loop:add! event-loop a)
    (rove:ok (= 5 (pantalea.event-loop:cell-event-result a)))
    (rove:ok (= 7 (pantalea.event-loop:cell-event-result b)))
    (pantalea.event-loop:stop! event-loop)
    (rove:ok (not (running-p event-loop)))))
