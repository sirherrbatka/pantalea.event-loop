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
  (let ((*context* (cons event *context*)))
    (handler-case
        (call-next-method)
      (error (e)
        (log-warn "~a" e)))))

(defmethod react ((event function) (loop event-loop))
  (funcall event))

(defmethod react ((event p:promise) (loop event-loop))
  (p:fullfill! event))

(defmethod react ((event event) (loop event-loop))
  (react-with-handler (obtain-handler event loop) event loop))

(defmethod react-with-handler ((handler response-handler)
                               (event response-handler)
                               (loop event-loop))
  (let ((*context* (cons event (context handler))))
    (p:fullfill! (request handler) (data handler))))

(defmethod setup-handler ((event request-event) (loop event-loop) data)
  (let ((response-handler (make-instance 'response-handler
                                         :request event
                                         :data data
                                         :context *context*)))
    (setf (gethash (id event) (request-handlers loop)) response-handler)))

(defmethod react ((event request-event) (loop event-loop))
  (setup-handler event loop (p:call/no-fullfill! event)))

(defmethod react ((event termination-event) (loop event-loop))
  (signal (make-condition 'termination-condition)))

(defmethod add! ((event-loop event-loop) event &optional (delay 0))
  (assert (>= 0 delay))
  (if (zerop delay)
      (queue-push! (queue event-loop) event)
      (tw:add! (timing-wheel event-loop)
               delay
               (lambda ()
                 (add! event-loop event)))))

(defmethod obtain-handler ((event event) (loop event-loop))
  (or (obtain-handler-with-id event loop (id event))
      (obtain-handler-without-id event loop)))

(defmethod obtain-handler ((event t) (loop event-loop))
  nil)

(defmethod start! ((event-loop event-loop))
  (bt2:with-lock-held ((main-lock event-loop))
    (when (thread event-loop)
      (error "EVENT-LOOP is already running!"))
    (setf (thread event-loop)
          (bt2:make-thread (lambda (&aux (queue event-loop) (*event-loop* event-loop))
                             (log-info "Event loop started.")
                             (handler-case
                                 (iterate
                                   (for event = (queue-pop! queue))
                                   (react event event-loop))
                               (termination-condition (e)
                                 (declare (ignore e))
                                 (log-info "Event loop thread recieved TERMINATION-CONDITION, will stop."))
                               (error (e)
                                 (log-error "Event loop thread crashing under error: ~a" e)))))
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
