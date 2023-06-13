(use-modules
 (shepherd support))

(define xss-lock
  (service '(xss-lock)
    #:respawn? #t
    #:start (make-forkexec-constructor '("xss-lock" "--" "slock"))
    #:stop (make-kill-destructor)))

(register-services (list xss-lock))
