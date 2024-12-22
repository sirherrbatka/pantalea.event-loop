(cl:in-package #:pantalea.event-loop)


(defmethod react ((event function) (loop event-loop))
  (handler-case (funcall event)
    (error (e)
      (log-warn "~a" e))))

(defmethod react ((event p:promise) (loop event-loop))
  (handler-case (p:fullfill! event)
    (error (e)
      (log-warn "~a" e))))

(defmethod react ((event event) (loop event-loop))
  (handler-case (react-with-handler (obtain-handler event loop) event loop)
    (error (e)
      (log-warn "~a" e))))

(defmethod react ((event request-event) (loop event-loop))
  (p:call/no-fullfill! event)
  (setup-handler event loop))

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
          (timing-wheel event-loop) (tw:run 500 0.01))
    event-loop)))

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
