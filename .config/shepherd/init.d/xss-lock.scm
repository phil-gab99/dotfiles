(define xss-lock
  (make <service>
    #:provides '(xss-lock)
    #:respawn? #t
    #:start (make-forkexec-constructor '("xss-lock" "--" "slock"))
    #:stop (make-kill-destructor)))

(register-services xss-lock)
