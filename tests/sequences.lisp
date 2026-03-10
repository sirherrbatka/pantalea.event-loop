(cl:in-package #:pantalea.event-loop)


(rove:deftest nested-events-sequence
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (with-existing-events-sequence
             (with-new-events-sequence
                 event-loop
                 ((a (:delay 0)
                     5))
               (add-cell-event! a))
             event-loop
             ((b (:success (a) :delay 0)
                 (+ a 2)))
           (rove:ok (= 5 (pantalea.event-loop:cell-event-result a)))
           (rove:ok (= 7 (pantalea.event-loop:cell-event-result b))))
      (pantalea.event-loop:stop! event-loop))))

#+(or)
(rove:run-test 'nested-events-sequence)

(rove:deftest two-elements-sequence-test
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (with-new-events-sequence
             event-loop
             ((a (:delay 3)
                 5)
              (b (:success (a) :delay 5)
                 (+ 2 a)))
           (pantalea.event-loop:add! event-loop a)
           (rove:ok (= 5 (pantalea.event-loop:cell-event-result a)))
           (rove:ok (= 7 (pantalea.event-loop:cell-event-result b))))
      (pantalea.event-loop:stop! event-loop))))

#+(or)
(rove:run-test 'two-elements-sequence-test)

(rove:deftest two-elements-error-test
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (with-new-events-sequence
             event-loop
             ((a ()
                 (error "BOOM!!!"))
              (b (:failure (a))
                 (handler-case a
                   (:no-error (e) (declare (ignore e)) 2)
                   (error (e) (declare (ignore e)) 1))))
           (pantalea.event-loop:add! event-loop a)
           (rove:ok (= 1 (cell-event-result b))))
      (pantalea.event-loop:stop! event-loop))))

#+(or)
(rove:run-test 'two-elements-error-test)

(rove:deftest two-elements-cancel-test
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop))) ; creates event-loop
    (pantalea.event-loop:start! event-loop) ; starts event-loop
    (rove:ok (running-p event-loop)) ; confirms that event-loop is running
    (unwind-protect
         (with-new-events-sequence ; syntax sugar on creating sequence of co-dependent events
             event-loop ; on this event-loop
             ((a (:delay 3) ; defines event A, which will be executed 2 seconds after being scheduled
                 5) ; this event produces 5 as a result
              (b (:success (a) :delay 5) ; this event will be scheduled 5 seconds after event a completes execution
                 (+ 2 a))) ; will automatically extract value from event a
           (add-cell-event! a) ; schedules "root" event
           (cancel! b (errors:make-chained event-loop-error ("canceled!"))) ; cancels b before it can be scheduled on the event-loop
           (rove:ok (= 5 (pantalea.event-loop:cell-event-result a))) ; a completes fine, this is a blocking operation
           (sleep 5) ; wait 5 seconds
           (rove:signals (pantalea.event-loop:cell-event-result b))) ; b was canceled before it was ever scheduled
      (pantalea.event-loop:stop! event-loop)))) ; stops the event loop, this is a blocking operation

#+(or)
(rove:run-test 'two-elements-cancel-test)

(rove:deftest cancel-after-completion-test
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (with-new-events-sequence
             event-loop
             ((a (:delay 0)
                 5)
              (b (:success (a) :delay 0)
                 (+ 2 a)))
           (add-cell-event! a)
           (sleep 3)
           (rove:signals (cancel! b (errors:make-chained event-loop-error ("canceled!"))))
           (rove:ok (= 5 (pantalea.event-loop:cell-event-result a)))
           (rove:ok (= 7 (pantalea.event-loop:cell-event-result b))))
      (pantalea.event-loop:stop! event-loop))))

#+(or)
(rove:run-test 'cancel-after-completion-test)

(rove:deftest request-sequence-test
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (with-new-events-sequence
             event-loop
             ((a (:timeout 10)
                 (respond 5)
                 (lambda (event)
                   (+ 2 (data event)))))
           (pantalea.event-loop:add! event-loop a)
           (rove:ok (= 7 (pantalea.event-loop:cell-event-result a))))
      (ignore-errors (stop! event-loop)))))

#+(or)
(rove:run-test 'request-sequence-test)

(rove:deftest request-sequence-test-2
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (with-new-events-sequence
             event-loop
             ((a (:timeout 0.5) ; 500 ms timeout
                 (lambda (event) (+ 2 (data event))))) ; handler for response, ignored
           (pantalea.event-loop:add! event-loop a) ; schedule event
           (sleep 1) ; make sure it expires
           (respond 5 a) ; respond on expired request, does nothing
           (rove:signals (pantalea.event-loop:cell-event-result a))) ; this signals "timeout" error
      (ignore-errors (stop! event-loop)))))

#+(or)
(rove:run-test 'request-sequence-test-2)

(rove:deftest request-sequence-test-3
  (let ((event-loop (make-instance 'pantalea.event-loop:event-loop)))
    (pantalea.event-loop:start! event-loop)
    (rove:ok (running-p event-loop))
    (unwind-protect
         (with-new-events-sequence
             event-loop
             ((a (:timeout 0.5) ; 500 ms timeout
                 (lambda (event) (+ 2 (data event)))) ; handler for response, ignored because response wont arrive in time
              (b (:failure (a)) ; this request will timeout
                 t)) ; signals
           (pantalea.event-loop:add! event-loop a) ; schedule event
           (sleep 1) ; make sure it expires
           (respond 5 a) ; respond on expired request, does nothing
           (rove:signals (pantalea.event-loop:cell-event-result a)) ; this signals "timeout" error
           (rove:ok (pantalea.event-loop:cell-event-result b))) ; timeout of a caused b to be executed, producing T as result
      (ignore-errors (stop! event-loop)))))

#+(or)
(rove:run-test 'request-sequence-test-3)

(rove:deftest conflicting-dependency
  (rove:ok (rove:signals (macroexpand
                          '(expand-cell-event-attach
                            (b (:success (a) :delay 5 :failure (a))
                             (+ 2 a)))))))

#+(or)
(rove:run-test 'conflicting-dependency)

(rove:deftest duplicated-dependency
  (rove:ok (rove:signals (macroexpand
                          '(expand-cell-event-attach
                            (b (:success (a a) :delay 5)
                             (+ 2 a)))))))

#+(or)
(rove:run-test 'duplicated-dependency)
