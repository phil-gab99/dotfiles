(define xsettingsd
  (make <service>
    #:provides '(xsettingsd)
    #:docstring "Runs `xsettingsd'"
    #:respawn? #f
    #:start (make-forkexec-constructor '("xsettingsd"))
    #:stop (make-kill-destructor)))

(register-services xsettingsd)
(start xsettingsd)
