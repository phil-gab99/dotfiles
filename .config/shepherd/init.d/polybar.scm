(define polybar
  (make <service>
    #:provides '(polybar)
    #:docstring "Runs `polybar'"
    #:respawn? #t
    #:start (make-system-constructor "polybar panel")
    #:stop (make-kill-destructor)))

(register-services polybar)
