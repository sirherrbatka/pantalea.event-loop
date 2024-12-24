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


(defmacro define-sequence (spec &body body)
  (let ((variable-names (mapcar #'first spec)))
    `(let (,@variable-names)
       (declare (ignorable ,@variable-names))
       ,@(mapcar (lambda (spec &aux
                            (arguments (second spec))
                            (body (cddr spec))
                            (variable-name (first spec))
                            (success-list (getf arguments :success))
                            (failure-list (getf arguments :failure))
                            (timeout (getf arguments :timeout))
                            (delay (getf arguments :delay)))
                   (assert (endp (intersection success-list failure-list)))
                   `(setf ,variable-name
                          ,(if timeout
                               `(make-instance 'request-event
                                 :callback (lambda () ,@body)
                                 :delay (or ,delay 0)
                                 :timeout ,timeout)
                               `(make-instance 'cell-event
                                 :callback (lambda () ,@body)
                                 :delay (or ,delay 0)))))
                 spec)
       ,@(mapcar
          (lambda (spec &aux
                     (arguments (second spec))
                     (name (first spec))
                     (success-list (getf arguments :success))
                     (failure-list (getf arguments :failure)))
            `(progn
               ,@(mapcar (lambda (d) `(attach-on-success! ,d ,name))
                         success-list)
               ,@(mapcar (lambda (d) `(attach-on-failure! ,d ,name))
                         failure-list)))
          spec)
       ,@body)))
