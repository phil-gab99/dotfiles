(define xss-lock
  (service '(xss-lock)
    #:documentation "Runs `xss-lock'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("xss-lock" "--" "slock"))
    #:stop (make-kill-destructor)))
