(cl:in-package #:pantalea.event-loop)


(rove:deftest two-elements-sequence-test
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (pantalea.event-loop:events-sequence
             ((a (:delay 3)
                 5)
              (b (:success (a) :delay 5)
                 (+ 2 a)))
           (pantalea.event-loop:add! event-loop a)
           (rove:ok (= 5 (pantalea.event-loop:cell-event-result a)))
           (rove:ok (= 7 (pantalea.event-loop:cell-event-result b))))
      (pantalea.event-loop:stop! event-loop))))

(rove:deftest request-sequence-test
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (pantalea.event-loop:events-sequence
             ((a (:timeout 10)
                 (add! event-loop (make-instance 'response-event
                                                 :id (id *event*)
                                                 :data 5))
                 (lambda (event)
                   (+ 2 (data event)))))
           (pantalea.event-loop:add! event-loop a)
           (rove:ok (= 7 (pantalea.event-loop:cell-event-result a))))
      (ignore-errors (stop! event-loop)))))
