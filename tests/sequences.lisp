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
                     (setf *test* 5)))
               (add-cell-event! a))
             event-loop
             (a)
             ((b (:success (a) :delay 0)
                 (+ a 2)))
           (rove:ok (= 5 (pantalea.event-loop:cell-event-result a)))
           (rove:ok (= 7 (pantalea.event-loop:cell-event-result b))))
      (pantalea.event-loop:stop! event-loop))))

#+(or)
(rove:run-test 'nested-events-sequence)


#|
(rove:deftest two-elements-sequence-test ;
(let ((event-loop (make-instance 'pantalea.event-loop:event-loop))) ;
(pantalea.event-loop:start! event-loop) ;
(rove:ok (running-p event-loop))        ;
(unwind-protect                         ;
(pantalea.event-loop:events-sequence    ;
event-loop                              ;
((a (:delay 3)                          ;
5)                                      ;
(b (:success (a) :delay 5)              ;
(+ 2 a)))                               ;
(pantalea.event-loop:add! event-loop a) ;
(rove:ok (= 5 (pantalea.event-loop:cell-event-result a))) ;
(rove:ok (= 7 (pantalea.event-loop:cell-event-result b)))) ;
(pantalea.event-loop:stop! event-loop)))) ;
                                        ;
(rove:deftest two-elements-cancel-test  ;
(let ((event-loop (make-instance 'pantalea.event-loop:event-loop))) ;
(pantalea.event-loop:start! event-loop) ;
(rove:ok (running-p event-loop))        ;
(unwind-protect                         ;
(pantalea.event-loop:events-sequence    ;
event-loop                              ;
((a (:delay 3)                          ;
5)                                      ;
(b (:success (a) :delay 5)              ;
(+ 2 a)))                               ;
(add-cell-event! a)                     ;
(cancel! b (errors:make-chained event-loop-error ("canceled!"))) ;
(rove:ok (= 5 (pantalea.event-loop:cell-event-result a))) ;
(rove:signals (pantalea.event-loop:cell-event-result b))) ;
(pantalea.event-loop:stop! event-loop)))) ;
                                        ;
(rove:deftest cancel-after-completion-test ;
(let ((event-loop (make-instance 'pantalea.event-loop:event-loop))) ;
(pantalea.event-loop:start! event-loop) ;
(rove:ok (running-p event-loop))        ;
(unwind-protect                         ;
(pantalea.event-loop:events-sequence    ;
event-loop                              ;
((a (:delay 0)                          ;
5)                                      ;
(b (:success (a) :delay 0)              ;
(+ 2 a)))                               ;
(add-cell-event! a)                     ;
(sleep 3)                               ;
(rove:signals (cancel! b (errors:make-chained event-loop-error ("canceled!")))) ;
(rove:ok (= 5 (pantalea.event-loop:cell-event-result a))) ;
(rove:ok (= 7 (pantalea.event-loop:cell-event-result b)))) ;
(pantalea.event-loop:stop! event-loop)))) ;
                                        ;
(rove:deftest request-sequence-test     ;
(let ((event-loop (make-instance 'pantalea.event-loop:event-loop))) ;
(pantalea.event-loop:start! event-loop) ;
(rove:ok (running-p event-loop))        ;
(unwind-protect                         ;
(pantalea.event-loop:events-sequence    ;
event-loop                              ;
((a (:timeout 10)                       ;
(add! event-loop (make-instance 'response-event ;
:id (id *event*)                        ;
:data 5))                               ;
(lambda (event)                         ;
(+ 2 (data event)))))                   ;
(pantalea.event-loop:add! event-loop a) ;
(rove:ok (= 7 (pantalea.event-loop:cell-event-result a)))) ;
(ignore-errors (stop! event-loop)))))   ;
                                        ;
(rove:deftest conflicting-dependency    ;
(rove:ok (rove:signals (macroexpand     ;
'(pantalea.event-loop:events-sequence   ;
event-loop                              ;
((a (:delay 3)                          ;
5)                                      ;
(b (:success (a) :delay 5 :failure (a)) ;
(+ 2 a))))))))                          ;
                                        ;
(rove:deftest duplicated-dependency     ;
(rove:ok (rove:signals (macroexpand     ;
'(pantalea.event-loop:events-sequence   ;
event-loop                              ;
((a (:delay 3)                          ;
5)                                      ;
(b (:success (a a) :delay 5)            ;
(+ 2 a))))))))                          ;
|#
