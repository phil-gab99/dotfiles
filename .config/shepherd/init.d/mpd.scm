(use-modules
 (shepherd support))

(define mpd
  (service '(mpd)
    #:documentation "Runs `mpd'"
    #:respawn? #t
    #:start (make-system-constructor "mpd")
    #:stop (make-system-destructor "mpd --kill")))

(register-services (list mpd))
;; (start-in-the-background '(mpd))
