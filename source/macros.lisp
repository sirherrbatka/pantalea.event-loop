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


(defun make-sequence-link-callback (promise delay main-list)
  (let ((lock (bt2:make-lock))
        (list (copy-list main-list)))
    (lambda (&optional p:*value*)
      (bt2:with-lock-held (lock)
        (when (progn
                (setf list (delete (first p:*promises*) list :test #'eq))
                (endp list))
          (add! *event-loop* promise (or delay 0))
          (setf list (copy-list main-list)))))))

(defmacro define-sequence (spec &body body)
  (bind ((variable-names (mapcar #'first spec)))
    `(let (,@variable-names)
       (declare (ignorable ,@variable-names))
       ,@(mapcar (lambda (spec &aux (body (cddr spec)) (variable-name (first spec)))
                   `(setf ,variable-name
                          (make-instance 'p:locked-callback
                                         :callback (lambda (&optional (p:*value* nil p:*value-bound-p*))
                                                     ,@body)
                                         :result nil
                                         :successp nil
                                         :fullfilled nil)))
                 spec)
       ,@(mapcar
          (lambda (spec &aux
                     (dependency (second spec))
                     (name (first spec))
                     (success-list (getf  dependency :success))
                     (failure-list (getf  dependency :failure))
                     (delay (getf  dependency :delay))
                     (complete-list (append success-list failure-list)))
            (assert (endp (intersection success-list failure-list)))
            (if (endp complete-list)
                nil
                `(let ((callback
                         (make-sequence-link-callback ,name ,delay (list ,@complete-list))))
                   ,@(mapcar (lambda (d) `(p:attach-on-success! ,d callback))
                             success-list)
                   ,@(mapcar (lambda (d) `(p:attach-on-failure! ,d callback))
                             failure-list))))
          spec)
       ,@body)))
