(cl:defpackage #:pantalea.event-loop
  (:use #:cl #:iterate)
  (:import-from #:metabang.bind
                #:bind)
  (:import-from #:alexandria
                #:most-positive-fixnum
                #:if-let
                #:switch)
  (:import-from #:log4cl
                #:log-info
                #:log-debug
                #:log-error
                #:log-warn)
  (:local-nicknames
   (#:tw #:pantalea.timing-wheel))
  (:local-nicknames
   (#:q #:pantalea.queue))
  (:local-nicknames
   (#:p #:pantalea.promise))
  (:export
   #:event
   #:start!
   #:stop
   #:react
   #:react-with-handler
   #:obtain-handler
   #:obtain-handler-with-id
   #:obtain-handler-without-id
   #:event-loop
   #:*event-loop*))
