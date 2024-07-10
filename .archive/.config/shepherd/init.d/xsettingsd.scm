(define xsettingsd
  (service '(xsettingsd)
    #:documentation "Runs `xsettingsd'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("xsettingsd"))
    #:stop (make-kill-destructor)))
