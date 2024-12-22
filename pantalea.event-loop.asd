(asdf:defsystem #:pantalea.event-loop
  :name "event-loop"
  :depends-on (#:bordeaux-threads
               #:iterate
               #:metabang-bind
               #:alexandria
               #:pantalea.queue
               #:pantalea.timing-wheel
               #:log4cl
               #:pantalea.promise)
  :serial T
  :pathname "source"
  :components ((:file "package")
               (:file "generics")
               (:file "classes")
               (:file "variables")
               (:file "macros")
               (:file "methods")
               ))
