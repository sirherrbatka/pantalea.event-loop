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


(defgeneric timing-wheel (event-loop))
(defgeneric id (event))
(defgeneric add! (event-loop event &optional delay))
(defgeneric react (event event-loop))
(defgeneric react-with-handler (handler event event-loop))
(defgeneric obtain-handler (event event-loop))
(defgeneric obtain-handler-without-id (event event-loop))
(defgeneric start! (event-loop))
(defgeneric stop! (event-loop))
(defgeneric running-p (event-loop))
(defgeneric context (event-loop))
(defgeneric (setf context) (new-value event-loop))
(defgeneric setup-response-handler (event event-loop payload))
(defgeneric remove-handler (event event-loop))
(defgeneric cell-notify-failure (cell failed))
(defgeneric cell-notify-success (cell succeded))
(defgeneric attach-on-success! (cell dep))
(defgeneric attach-on-failure! (cell dep))
(defgeneric add-cell-event! (cell-event))
(defgeneric cell-event-result (cell-event))
(defgeneric cancel! (cell-event reason))
(defgeneric response-handler (loop id))
(defgeneric (setf response-handler) (new-value loop id))
(defgeneric hook (cell hooks))
