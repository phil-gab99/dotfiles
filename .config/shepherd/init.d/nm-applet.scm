(define nm-applet
  (make <service>
    #:provides '(nm-applet)
    #:docstring "Runs `nm-applet'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("nm-applet"))
    #:stop (make-kill-destructor)))

(register-services nm-applet)
