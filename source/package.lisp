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
                #:blocking-queue-push!
                #:blocking-queue-pop!)
  (:import-from #:serapeum
                #:nest
                #:dict
                #:lret
                #:~>>
                #:~>)
  (:import-from #:alexandria
                #:most-positive-fixnum
                #:if-let
                #:flatten
                #:once-only
                #:hash-table-values
                #:rcurry
                #:hash-table-keys
                #:eswitch
                #:with-gensyms
                #:when-let
                #:switch)
  (:local-nicknames
   (#:errors #:pantalea.errors))
  (:local-nicknames
   (#:tw #:pantalea.timing-wheel))
  (:local-nicknames
   (#:p #:pantalea.promise))
  (:export
   #:*event*
   #:*event-loop*
   #:add!
   #:attach-on-success!
   #:attach-on-failure!
   #:cell-event
   #:add-cell-event!
   #:cell-event-result
   #:cell-notify-failure
   #:cell-notify-success
   #:context
   #:event
   #:event-loop
   #:with-new-events-sequence
   #:with-existing-events-sequence
   #:obtain-handler
   #:obtain-handler-without-id
   #:on-event-loop
   #:react
   #:react-with-handler
   #:remove-response-handler
   #:request-event
   #:response-handler
   #:defhook
   #:response-handler
   #:running-p
   #:setup-response-handler
   #:start!
   #:stop!
   #:respond
   #:make-event
   #:callback
   ))
