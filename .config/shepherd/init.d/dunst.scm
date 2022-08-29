(define dunst
  (make <service>
    #:provides '(dunst)
    #:docstring "Runs `dunst'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("dunst"))
    #:stop (make-kill-destructor)))

(register-services dunst)
