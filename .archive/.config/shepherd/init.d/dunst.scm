(define dunst
  (service '(dunst)
    #:documentation "Runs `dunst'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("dunst"))
    #:stop (make-kill-destructor)))
