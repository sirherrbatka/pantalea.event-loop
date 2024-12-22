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
                   `(setf ,variable-name (make-instance 'p:locked-callback
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
