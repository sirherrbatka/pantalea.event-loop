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

(defmacro make-event (name (&rest all
                            &key
                              success failure
                              (timeout nil timeout-bound-p)
                              (delay 0)
                              (start-deadline nil)
                              (class (if timeout-bound-p 'request-event 'cell-event))
                              (event-loop nil event-loop-bound-p) )
                      &body body)
  (let* ((everything (append success failure))
         (gensyms (map-into (copy-list everything) #'gensym)))
    `(lret ((,name
             (make-instance ',class
                            :delay ,delay
                            :success-dependencies '(,@success)
                            :failure-dependencies '(,@failure)
                            :event-loop ,(if event-loop-bound-p event-loop *event-loop*)
                            :name ',name
                            :start-deadline ,start-deadline
                            :callback (lambda (&aux (*event* *event*))
                                        (let ,(mapcar #'list gensyms everything)
                                          (symbol-macrolet ,(mapcar (lambda (symbol gensym)
                                                                      `(,symbol (cell-event-result ,gensym)))
                                                             everything
                                                             gensyms)
                                            ,@body)))
                            ,@(when timeout-bound-p (list :timeout timeout)))))
       (expand-cell-event-attach ((,name (,@all)))))))

(defmacro expand-cell-event (variable-name (&key success failure
                                              (timeout nil timeout-bound-p)
                                              (delay 0)
                                              (start-deadline nil)
                                              (class (if timeout-bound-p 'request-event 'cell-event))
                                              (event-loop nil event-loop-bound-p))
                             &body body)
  (bind ((everything (append success failure))
         (gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym)) everything)))
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
                                      (let ,(mapcar #'list gensyms everything)
                                        (symbol-macrolet ,(mapcar (lambda (symbol gensym)
                                                                    `(,symbol (cell-event-result ,gensym)))
                                                           everything
                                                           gensyms)
                                          ,@body)))
                          ,@(when timeout-bound-p (list :timeout timeout))))))

(defmacro expand-cell-event-attach (spec)
  `(progn
     ,@(apply #'append
              (mapcar (lambda (spec)
                        (bind (((name (&key success failure &allow-other-keys). body) spec))
                          (declare (ignore body))
                          (assert (endp (intersection success failure)))
                          (assert (equal (remove-duplicates success) success))
                          (assert (equal (remove-duplicates failure) failure))
                          `(,@(mapcar (lambda (d)
                                        `(attach-on-success! ,d ,name))
                                      success)
                            ,@(mapcar (lambda (d)
                                        `(attach-on-failure! ,d ,name))
                                      failure))))
                      spec))))

(defmacro with-events (spec event-loop &body body)
  (bind (((:flet variable-name (spec)) (first spec))
         (variable-names (mapcar #'variable-name spec)))
    (once-only (event-loop)
      `(let ((*event-loop* ,event-loop)
             ,@variable-names)
         ,@(mapcar (lambda (spec)
                     `(expand-cell-event ,@spec))
                   spec)
         (expand-cell-event-attach ,spec)
         ,@body))))

(defmacro with-new-events-sequence (event-loop (&rest spec) &body body)
  (bind (((:flet variable-name (spec)) (first spec))
         (variable-names (mapcar #'variable-name spec)))
    `(with-events ,spec ,event-loop
       (lret ((result
               (make-instance 'events-sequence
                              :contained-events (dict ,@(iterate
                                                          (for variable-name in variable-names)
                                                          (collecting `',variable-name)
                                                          (collecting variable-name)))
                              :event-loop ,event-loop
                              :callback (lambda (&aux (*event-loop* ,event-loop))
                                          ,@body))))
         (unless *events-context*
           (add-cell-event! result))))))

(defmacro with-existing-events-sequence (existing-events-sequence event-loop (&rest spec) &body body)
  (bind ((existing-events (set-difference (~>> spec
                                               (mapcar (lambda (spec)
                                                         (bind (((name (&key success failure &allow-other-keys). body) spec))
                                                           (declare (ignore body name))
                                                           (append success failure))))
                                               (apply #'append)
                                               remove-duplicates)
                                          (mapcar #'first spec))))
    (with-gensyms (!old-context !existing-events-sequence)
      `(let* ((,!old-context *events-context*)
              (*events-context* t)
              (,!existing-events-sequence ,existing-events-sequence)
              ,@(mapcar (lambda (name) `(,name (event-in-events-sequence ,!existing-events-sequence ',name)))
                        existing-events))
         (with-events ,spec ,event-loop
           (progn
             ,@(iterate
                 (for variable in spec)
                 (for variable-name = (first variable))
                 (collecting `(setf (gethash ',variable-name (contained-events ,existing-events-sequence))
                                    ,variable-name)))
             (unless ,!old-context
               (add-cell-event! ,!existing-events-sequence))
             ,@body))))))

(defmacro on-event-loop ((&key (delay 0) (event-loop '*event-loop*)) &body body)
  `(let ((*event-loop* ,event-loop))
     (add! *event-loop* (lambda () ,@body) ,delay)))
