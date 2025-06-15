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


(defmacro with-locked-context (additional-cells &body body)
  (with-gensyms (!events-context)
    `(let ((,!events-context *events-context*))
       (nest ,@(mapcar (lambda (cell) `(bt2:with-lock-held ((lock ,cell)))) additional-cells)
             (progn (map nil (lambda (x) (when x (bt2:acquire-lock (lock x)))) ,!events-context)
                    (unwind-protect ,@body
                      (map nil (lambda (x) (when x (bt2:release-lock (lock x)))) ,!events-context)))))))

(defmacro with-attach (new-cells &body body)
  `(with-locked-context (,@new-cells)
     (prog1 (progn ,@body)
       ,@(iterate
           (for cell in new-cells)
           (collecting
             `(clear-dependency ,cell))))))

(defmacro events-sequence (event-loop spec &body body)
  (let ((variable-names (mapcar #'first spec)))
    (alexandria:with-gensyms (!hook !cell)
      `(let* ((*events-context* (or *events-context* (list nil)))
              (*event-loop* ,event-loop)
              ,@variable-names)
         (declare (ignorable ,@variable-names))
         ,@(mapcar (lambda (spec)
                     (bind (((variable-name args . body) spec)
                            ((&key success failure
                                   (timeout nil timeout-bound-p)
                                   (delay 0)
                                   (start-deadline nil)
                                   (class (if timeout-bound-p 'request-event 'cell-event))
                                   (event-loop nil event-loop-bound-p))
                             args)
                            (everything (append success failure))
                            (combined (intersection everything variable-names))
                            (foreign (set-difference everything variable-names))
                            (gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym))
                                             combined)))
                       (assert (endp (intersection success failure)))
                       (assert (equal (remove-duplicates everything) everything))
                       `(setf ,variable-name
                              (make-instance ',class
                                             :delay ,delay
                                             :success-dependencies '(,@success)
                                             :failure-dependencies '(,@failure)
                                             :event-loop ,(if event-loop-bound-p event-loop *event-loop*)
                                             :name ',variable-name
                                             :start-deadline ,start-deadline
                                             :callback (lambda (&aux (*event* *event*))
                                                         (symbol-macrolet ,(mapcar (lambda (symbol)
                                                                                     `(,symbol (cell-event-result (find ',symbol
                                                                                                                        (dependency-cells *event*)
                                                                                                                        :key #'name))))
                                                                            foreign)
                                                           (let ,(mapcar #'list gensyms combined)
                                                             (symbol-macrolet ,(mapcar (lambda (gensym symbol)
                                                                                         `(,symbol (cell-event-result ,gensym)))
                                                                                gensyms
                                                                                combined)
                                                               ,@body))))
                                             ,@(when timeout-bound-p (list :timeout timeout))))))
                   spec)
         (with-attach ,variable-names
           (iterate
             (for ,!cell in *events-context*)
             (until (null ,!cell))
             (iterate
               (for ,!hook in (success-dependencies ,!cell))
               (cond ,@(mapcar (lambda (symbol)
                                 `((eq ',symbol ,!hook)
                                   (attach-on-success! ,symbol ,!cell)
                                   (push ,symbol (dependency-cells ,!cell))))
                               variable-names)))
             (iterate
               (for ,!hook in (failure-dependencies ,!cell))
               (cond ,@(mapcar (lambda (symbol)
                                 `((eq ',symbol ,!hook)
                                   (attach-on-failure! ,symbol ,!cell)
                                   (push ,symbol (dependency-cells ,!cell))))
                               variable-names))))
           ,@(mapcar
              (lambda (symbol)
                `(push ,symbol *events-context*))
              variable-names)
           ,@(mapcar
              (lambda (spec)
                (bind (((name args . body) spec)
                       ((&key success  failure &allow-other-keys) args))
                  (declare (ignore body))
                  `(progn
                     ,@(mapcar (lambda (d)
                                 (when (member d variable-names)
                                   `(attach-on-success! ,d ,name)))
                               success)
                     ,@(mapcar (lambda (d)
                                 (when (member d variable-names)
                                   `(attach-on-failure! ,d ,name)))
                               failure))))
              spec))
         ,@body))))

(defmacro on-event-loop ((&key (delay 0) (event-loop '*event-loop*)) &body body)
  `(let ((*event-loop* ,event-loop))
     (add! *event-loop* (lambda () ,@body) ,delay)))

(defmacro defhook (name)
  `(defconstant ,name ',name))
