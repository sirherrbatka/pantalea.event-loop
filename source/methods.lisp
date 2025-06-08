#|
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2) Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(cl:in-package #:pantalea.event-loop)


(defmethod id ((handler response-handler))
  (id (request handler)))

(defmethod react :around ((event t) (loop event-loop))
  (log:info "Reacting to event ~a" event)
  (handler-case
      (call-next-method)
    (error (e)
      (log:warn "~a" e))))

(defmethod react ((event function) (loop event-loop))
  (funcall event))

(defmethod react ((event p:promise) (loop event-loop))
  (p:fullfill! event))

(defmethod react ((event event) (loop event-loop))
  (setf (completed event) t)
  (react-with-handler (obtain-handler event loop) event loop))

(defmethod react-with-handler ((handler (eql nil))
                               (event response-event)
                               (loop event-loop))
  (errors:!!! no-handler-error ("No handler for response-event ~a" event)))

(defmethod react-with-handler ((handler response-handler)
                               (event response-event)
                               (loop event-loop))
  (handler-case
      (p:fullfill! (promise (request handler)) (funcall (payload handler) event))
    (:no-error (e) (declare (ignore e))
      (iterate
          (for elt in (success-dependent event))
          (cell-notify-success elt event)))
    (error (e)
      (handler-case (p:cancel! (request handler)
                               :condition e
                               :timeout 0.1)
        (error (e)
          (log:warn "Error while attempting to cancel: ~a" e))
        (:no-error (e) (declare (ignore e))
          (iterate
            (for elt in (bt2:with-lock-held ((lock event))
                          (failure-dependent event)))
            (cell-notify-failure elt event)))))))

(defmethod response-handler ((loop event-loop) id)
  (gethash id (request-handlers loop)))

(defmethod (setf response-handler) (new-value (loop event-loop) id)
  (setf (gethash id (request-handlers loop)) new-value))

(defmethod setup-response-handler ((event request-event) (loop event-loop) payload)
  (let ((handler (make-instance 'response-handler
                           :request event
                           :payload payload)))
    (bt2:with-lock-held ((main-lock loop))
      (setf (response-handler loop (id event)) handler))))

(defmethod react :around ((event cell-event) (loop event-loop))
  (bind (((:accessors lock promise failure-dependent) event))
    (when (p:fullfilledp promise)
      ;; since this could not be run before, it had to be canceled, we will simply notify failure
      (iterate
        (for elt in (failure-dependent event))
        (cell-notify-failure elt event))
      (return-from react nil))
    (call-next-method)))

(defmethod remove-response-handler ((event event) (loop event-loop))
  (bt2:with-lock-held ((main-lock loop))
    (remhash (id event) (request-handlers loop))))

(defmethod react ((event request-event) (loop event-loop))
  (handler-case
      (bind ((*event* event)
             ((:accessors timeout lock callback promise) event)
             (response-handler-payload (funcall callback))
             ((:slots (failure-dependent %failure-dependent)
                      (success-dependent %success-dependent))
              event))
        (bt2:with-lock-held (lock)
          (setup-response-handler event loop response-handler-payload)
          (on-event-loop (:delay timeout)
            (unless (p:fullfilledp promise)
              (log:warn "Timeout while waiting on request ~a" event)
              (cancel! event (errors:make-chained request-timeout
                                                  ("Timeout ~a crossed." timeout)))))))
    (error (e)
      (handler-case (p:cancel! (promise event)
                               :condition e
                               :timeout 0.1)
        (error (e)
          (log:warn "While attempting canceling: ~a" e))
        (:no-error (e) (declare (ignore e))
          (iterate
            (for elt in (failure-dependent event))
            (cell-notify-failure elt event))))
      (error e))))

(defmethod react ((event cell-event) (loop event-loop))
  (bind ((*event* event)
         ((:slots (failure-dependent %failure-dependent)
                  (success-dependent %success-dependent))
          event)
         ((:accessors lock callback promise) event))
    (bt2:with-lock-held (lock)
      (handler-case
          (p:fullfill! promise (funcall callback))
        (:no-error (e) (declare (ignore e))
          (iterate
            (for elt in success-dependent)
            (cell-notify-success elt event)))
        (error (e)
          (handler-case (p:cancel! (promise event)
                                   :condition e
                                   :timeout 0.1)
            (error (e)
              (log:warn "While attempting canceling: ~a" e))
            (:no-error (e) (declare (ignore e))
              (iterate
                (for elt in failure-dependent)
                (cell-notify-failure elt event))))
          (error e))))))

(defmethod add-cell-event! ((event cell-event))
  (add! (or (event-loop event) *event-loop*) event))

(defmethod cell-notify-failure ((cell cell-event) failed)
  (handler-case
      (bind (((:accessors lock) cell)
             ((:slots %dependency) cell))
        (bt2:with-lock-held (lock)
            (setf %dependency (delete failed %dependency)))
        (when (endp %dependency)
          (add-cell-event! cell)))
    (error (e)
      (log:warn "~a" e))))

(defmethod cell-notify-success ((cell cell-event) failed)
  (handler-case
      (bind (((:accessors lock) cell)
             ((:slots %dependency) cell))
        (bt2:with-lock-held (lock)
            (setf %dependency (delete failed %dependency)))
        (when (endp %dependency)
          (add-cell-event! cell)))
    (error (e)
      (log:warn "~a" e))))

(defmethod react ((event termination-event) (loop event-loop))
  (signal (make-condition 'termination-condition)))

(defmethod cell-event-result ((event cell-event) &optional timeout (loop t))
  (p:force (promise event) :timeout timeout :loop loop))

(defmethod cancel! ((event cell-event) reason)
  (errors:with-link (errors:!!! unable-to-cancel ("Can't cancel cell.")) (unable-to-cancel)
    (p:cancel! (promise event)
               :condition reason
               :timeout 0.1))
  event)

(defmethod cancel! ((event request-event) reason)
  (handler-case (call-next-method)
    (unable-to-cancel (e)
      (log:warn "Unable to cancel request, removing handlers regardless. ~a" e)
      (ignore-errors (remove-response-handler event reason)))
    (:no-error (e) (declare (ignore e))
      (ignore-errors (remove-response-handler event reason)))))

(defmethod add! ((event-loop event-loop) event &optional (delay 0))
  (assert (>= delay 0))
  (bt2:with-lock-held ((main-lock event-loop))
    (unless (thread event-loop)
      (errors:!!! event-loop-not-started ("EVENT-LOOP is not running!")))
    (if (zerop delay)
        (blocking-queue-push! (queue event-loop) event)
        (tw:add! (timing-wheel event-loop)
                 delay
                 (lambda ()
                   (ignore-errors (add! event-loop event 0))))))
  event)

(defmethod add! ((event-loop event-loop) (event cell-event) &optional (delay (delay event)))
  (assert (>= delay 0))
  (bt2:with-lock-held ((main-lock event-loop))
    (unless (thread event-loop)
      (errors:!!! event-loop-not-started ("EVENT-LOOP is not running!")))
    (if (zerop delay)
        (progn
          (blocking-queue-push! (queue event-loop) event))
        (progn
          (tw:add! (timing-wheel event-loop)
                   delay
                   (lambda ()
                     (add! event-loop event 0)))))
    (when-let ((start-deadline (start-deadline event)))
      (add! event-loop
            start-deadline
            (lambda ()
              (ignore-errors
               (p:cancel! (promise event)
                          :condition (errors:make-chained not-started-before-deadline
                                                          ("Task was not started before the START-deadline ~a" start-deadline))
                          :timeout 0.1))))))
  event)

(defmethod obtain-handler-without-id ((event t) (loop event-loop))
  nil)

(defmethod obtain-handler ((event event) (loop event-loop))
  (or (response-handler loop (id event))
      (obtain-handler-without-id event loop)))

(defmethod obtain-handler ((event t) (loop event-loop))
  nil)

(defmethod start! ((event-loop event-loop))
  (bt2:with-lock-held ((main-lock event-loop))
    (when (thread event-loop)
      (errors:!!! event-loop-already-running ("EVENT-LOOP is already running!")))
    (setf (thread event-loop)
          (bt2:make-thread (lambda (&aux (queue (queue event-loop)) (*event-loop* event-loop))
                             (log:info "Event loop started.")
                             (handler-case
                                 (iterate
                                   (react (blocking-queue-pop! queue)
                                          event-loop))
                               (termination-condition (e)
                                 (declare (ignore e))
                                 (log:info "Event loop thread recieved TERMINATION-CONDITION, will stop."))
                               (error (e)
                                 (log:error "Event loop thread crashing under error: ~a" e)))))
          (timing-wheel event-loop) (tw:run 1000 0.01))
    event-loop))

(defmethod running-p ((event-loop event-loop))
  (bt2:with-lock-held ((main-lock event-loop))
    (not (null (thread event-loop)))))

(defmethod stop! ((event-loop event-loop))
  (bt2:with-lock-held ((main-lock event-loop))
    (unless (thread event-loop)
      (errors:!!! event-loop-not-started ("EVENT-LOOP is not running!")))
    (tw:stop! (timing-wheel event-loop))
    (blocking-queue-push! (queue event-loop) (make-instance 'termination-event))
    (bt2:join-thread (thread event-loop))
    (setf (thread event-loop) nil
          (queue event-loop) (make-blocking-queue)
          (timing-wheel event-loop) nil))
  event-loop)

(defmethod attach-on-success! ((cell cell-event) (dep cell-event))
  (push dep (slot-value cell '%success-dependent))
  (push cell (slot-value dep '%dependency)))

(defmethod attach-on-failure! ((cell cell-event) (dep cell-event))
  (push dep (slot-value cell '%failure-dependent))
  (push cell (slot-value dep '%dependency)))

(defmethod success-dependent :around ((event cell-event))
  (bt2:with-lock-held ((lock event)) (call-next-method)))

(defmethod (setf success-dependent) :around (new-value (event cell-event))
  (bt2:with-lock-held ((lock event)) (call-next-method)))

(defmethod failure-dependent :around ((event cell-event))
  (bt2:with-lock-held ((lock event)) (call-next-method)))

(defmethod (setf failure-dependent) :around (new-value (event cell-event))
  (bt2:with-lock-held ((lock event)) (call-next-method)))

(defmethod dependency :around ((event cell-event))
  (bt2:with-lock-held ((lock event)) (call-next-method)))

(defmethod (setf dependency) :around (new-value (event cell-event))
  (bt2:with-lock-held ((lock event)) (call-next-method)))
