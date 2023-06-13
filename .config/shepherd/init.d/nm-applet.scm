(use-modules
 (shepherd support))

(define nm-applet
  (service '(nm-applet)
    #:documentation "Runs `nm-applet'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("nm-applet"))
    #:stop (make-kill-destructor)))

(register-services (list nm-applet))
