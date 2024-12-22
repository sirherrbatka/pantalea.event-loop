(cl:in-package #:pantalea.event-loop)


(defmethod react :around (event *event-loop*)
  (call-next-method))

(defmethod react ((event function) (loop event-loop))
  (handler-case (funcall event)
    (error (e)
      (log-warn "~a" e))))

(defmethod react ((event event) (loop event-loop))
  (react-with-handler (obtain-handler event loop) event loop))

(defmethod add! ((event-loop event-loop) event &optional (delay 0))
  (assert (>= 0 delay))
  (if (zerop delay)
      (q:queue-push! (queue event-loop) event)
      (tw:add! (timing-wheel event-loop)
               delay
               (lambda ()
                 (add! event-loop event)))))

(defmethod obtain-handler ((event event) (loop event-loop))
  (or (obtain-handler-with-id event loop (id event))
      (obtain-handler-without-id event loop)))

(defmethod obtain-handler ((event t) (loop event-loop))
  nil)
