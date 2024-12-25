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
  (error "No handler for response-event ~a" event))

(defmethod react-with-handler ((handler response-handler)
                               (event response-event)
                               (loop event-loop))
  (iterate
    (for elt in (success-dependent event))
    (cell-notify-success elt event))
  (p:fullfill! (request handler) (data handler)))

(defmethod request-handler ((loop event-loop) id)
  (gethash id (request-handlers loop)))

(defmethod (setf request-handler) (new-value (loop event-loop) id)
  (setf (gethash id (request-handlers loop)) new-value))

(defmethod setup-response-handler ((event request-event) (loop event-loop) data)
  (setf (response-handler loop (id event))
        (make-instance 'response-handler
                       :request event
                       :data data)))

(defmethod react :around ((event cell-event) (loop event-loop))
  (bind (((:accessors lock promise canceled failure-dependent) event))
    (bt2:with-lock-held (lock)
      (when canceled
        (p:cancel! promise canceled)
        (iterate
          (for elt in failure-dependent)
          (cell-notify-failure elt event))
        (return-from react nil)))
    (call-next-method)))

(defmethod remove-response-handler ((event event) (loop event-loop))
  (remhash (id event) (request-handlers loop)))

(defmethod react ((event request-event) (loop event-loop))
  (handler-case
      (bind (((:accessors timeout completed lock) event))
        (setup-response-handler event loop (funcall (callback event)))
        (on-event-loop (:delay (timeout event))
          (unless (bt2:with-lock-held (lock) (completed event))
                  (log-warn "Timeout while waiting on request ~a" event)
                  (remove-response-handler event loop)
                  (cancel! event (make-condition 'timeout-error)))))
    (error (e)
      (iterate
        (for elt in (failure-dependent event))
        (cell-notify-failure elt event))
      (p:cancel! (promise event) e)
      (signal e))))

(defmethod react ((event cell-event) (loop event-loop))
  (handler-case
      (progn
        (p:fullfill! (promise event) (funcall (callback event)))
        (iterate
          (for elt in (success-dependent event))
          (cell-notify-success elt event)))
    (error (e)
      (iterate
        (for elt in (failure-dependent event))
        (cell-notify-failure elt event))
      (p:fullfill! (promise event) e)
      (signal e))))

(defmethod add-cell-event! ((event cell-event))
  (add! *event-loop* event (delay event)))

(defmethod cell-notify-failure ((cell cell-event) failed)
  (handler-case
      (bind (((:accessors dependency-init dependency) cell))
        (setf dependency (delete failed dependency))
        (when (endp dependency)
          (add-cell-event! cell)))
    (error (e)
      (log:warn "~a" e))))

(defmethod cell-notify-success ((cell cell-event) failed)
  (handler-case
      (bind (((:accessors dependency-init dependency) cell))
        (setf dependency (delete failed dependency))
        (when (endp dependency)
          (setf dependency (copy-list dependency-init))
          (add-cell-event! cell)))
    (error (e)
      (log:warn "~a" e))))

(defmethod react ((event termination-event) (loop event-loop))
  (signal (make-condition 'termination-condition)))

(defmethod cell-event-result ((event cell-event))
  (p:force (promise event)))

(defmethod cancel! ((event cell-event) reason)
  (bt2:with-lock-held ((lock event))
    (setf (canceled event) reason)))

(defmethod add! ((event-loop event-loop) event &optional (delay 0))
  (assert (>= delay 0))
  (if (zerop delay)
      (blocking-queue-push! (queue event-loop) event)
      (tw:add! (timing-wheel event-loop)
               delay
               (lambda ()
                 (add! event-loop event 0))))
  event-loop)

(defmethod add! ((event-loop event-loop) (event cell-event) &optional (delay (delay event)))
  (call-next-method event-loop event delay))

(defmethod obtain-handler-without-id ((event t) (loop event-loop))
  nil)

(defmethod obtain-handler ((event event) (loop event-loop))
  (or (response-handler loop (id event))
      (handler-without-id event loop)))

(defmethod obtain-handler ((event t) (loop event-loop))
  nil)

(defmethod start! ((event-loop event-loop))
  (bt2:with-lock-held ((main-lock event-loop))
    (when (thread event-loop)
      (error "EVENT-LOOP is already running!"))
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
      (error "EVENT-LOOP is not running!"))
    (tw:stop! (timing-wheel event-loop))
    (add! event-loop (make-instance 'termination-event))
    (bt2:join-thread (thread event-loop))
    (setf (thread event-loop) nil
          (queue event-loop) (make-blocking-queue)
          (timing-wheel event-loop) nil)
    event-loop))

(defmethod attach-on-success! ((cell cell-event) (dep cell-event))
  (push dep (success-dependent cell))
  (push cell (dependency dep))
  (push cell (dependency-init dep)))

(defmethod attach-on-failure! ((cell cell-event) (dep cell-event))
  (push dep (failure-dependent cell))
  (push cell (dependency dep))
  (push cell (dependency-init dep)))
