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

(cl:defpackage #:pantalea.event-loop
  (:use #:cl #:iterate)
  (:import-from #:metabang.bind
                #:bind)
  (:import-from #:pantalea.queue
                #:make-blocking-queue
                #:queue-push!
                #:queue-pop!)
  (:import-from #:alexandria
                #:most-positive-fixnum
                #:if-let
                #:flatten
                #:hash-table-values
                #:rcurry
                #:hash-table-keys
                #:eswitch
                #:switch)
  (:import-from #:log4cl
                #:log-info
                #:log-debug
                #:log-error
                #:log-warn)
  (:local-nicknames
   (#:tw #:pantalea.timing-wheel))
  (:local-nicknames
   (#:p #:pantalea.promise))
  (:export
   #:*event-loop*
   #:attach-on-failure!
   #:attach-on-success!
   #:cell-event
   #:cell-event-result
   #:cell-notify-failure
   #:cell-notify-success
   #:context
   #:event
   #:event-loop
   #:handler
   #:obtain-handler
   #:obtain-handler-without-id
   #:react
   #:react-with-handler
   #:request-event
   #:response-handler
   #:running-p
   #:start!
   #:stop!
   ))
