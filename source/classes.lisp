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


(defclass event-loop ()
  ((%context
    :initarg :context
    :accessor context)
   (%timing-wheel
    :initarg :timing-wheel
    :accessor timing-wheel)
   (%main-lock
    :initarg :main-lock
    :reader main-lock)
   (%thread
    :initarg :thread
    :accessor thread)
   (%request-handlers
    :initarg :request-handlers
    :reader request-handlers)
   (%general-handlers
    :initarg :general-handlers
    :reader general-handlers)
   (%queue
    :initarg :queue
    :accessor queue))
  (:default-initargs
   :thread nil
   :request-handlers (make-hash-table)
   :general-handlers (make-hash-table)
   :context nil
   :timing-wheel nil
   :main-lock (bt2:make-lock)
   :queue (make-blocking-queue)))

(defclass event ()
  ((%id
    :initarg :id
    :accessor id)
   (%completed
    :initarg :completed
    :accessor completed))
  (:default-initargs
   :completed nil
   :id (random most-positive-fixnum)))

(defclass cell-event (event)
  ((%promise :initarg :promise
             :accessor promise)
   (%callback :initarg :callback
              :reader callback)
   (%dependency :initarg :dependency
                :accessor dependency)
   (%delay :initarg :delay
           :reader delay)
   (%dependency-init :initarg :dependency-init
                     :accessor dependency-init)
   (%failure-dependent :initarg :failure-dependent
                       :accessor failure-dependent)
   (%success-dependent :initarg :success-dependent
                       :accessor success-dependent)
   (%lock :initarg :lock
          :reader lock)
   (%canceled :initarg :canceled
              :accessor canceled)
   (%event-loop :initarg :event-loop
                :reader event-loop))
  (:default-initargs
   :canceled nil
   :lock (bt2:make-lock)
   :event-loop *event-loop*
   :delay 0
   :dependency (list)
   :dependency-init (list)
   :promise (p:promise nil)
   :success-dependent (list)
   :failure-dependent (list)))

(defclass request-event (cell-event)
  ((%timeout
    :initarg :timeout
    :reader timeout))
  (:default-initargs
   :timeout nil))

(defclass response-event (event)
  ())

(defclass termination-event ()
  ())

(define-condition termination-condition ()
  ())

(define-condition timeout-error (error)
  ())

(defclass response-handler ()
  ((%request
    :initarg :request
    :reader request)
   (%data
    :initarg :data
    :reader data))
  (:default-initargs
   :context (list)))
