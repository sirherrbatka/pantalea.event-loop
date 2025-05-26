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


(defmacro events-sequence (spec &body body)
  (let ((variable-names (mapcar #'first spec)))
    `(let (,@variable-names)
       (declare (ignorable ,@variable-names))
       ,@(mapcar (lambda (spec)
                   (bind (((variable-name args . body) spec)
                          ((&key success failure (timeout nil timeout-bound-p) (delay 0) (class (if timeout-bound-p 'request-event 'cell-event)))
                           args)
                          (combined (append success failure))
                          (gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym))
                                           combined)))
                     (assert (endp (intersection success failure)))
                     `(setf ,variable-name
                            (make-instance ',class
                                           :delay ,delay
                                           :callback ,(if combined
                                                          `(lambda ()
                                                             (let ,(mapcar #'list gensyms combined)
                                                               (symbol-macrolet ,(mapcar (lambda (gensym symbol)
                                                                                           `(,symbol (cell-event-result ,gensym)))
                                                                                  gensyms
                                                                                  combined)
                                                                 ,@body)))
                                                          `(lambda () ,@body))
                                           ,@(when timeout-bound-p (list :timeout timeout))))))
                 spec)
       ,@(mapcar
          (lambda (spec)
            (bind (((name args . body) spec)
                   ((&key success  failure &allow-other-keys) args))
              (declare (ignore body))
              `(progn
                 ,@(mapcar (lambda (d) `(attach-on-success! ,d ,name))
                           success)
                 ,@(mapcar (lambda (d) `(attach-on-failure! ,d ,name))
                           failure))))
          spec)
       ,@body)))

(defmacro on-event-loop ((&key (delay 0) (event-loop '*event-loop*)) &body body)
  `(let ((*event-loop* ,event-loop))
     (add! *event-loop* (lambda () ,@body) ,delay)))
