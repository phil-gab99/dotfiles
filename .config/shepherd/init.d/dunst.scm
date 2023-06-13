(use-modules
 (shepherd support))

(define dunst
  (service '(dunst)
    #:documentation "Runs `dunst'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("dunst"))
    #:stop (make-kill-destructor)))

(register-services (list dunst))
