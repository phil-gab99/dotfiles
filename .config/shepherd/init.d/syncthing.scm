(define syncthing
  (service '(syncthing)
    #:documentation "Runs `syncthing'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing" "--no-browser"))
    #:stop (make-kill-destructor)))

(define syncthing-gtk
  (service '(syncthing-gtk)
    #:documentation "Runs `syncthing-gtk'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing-gtk" "--minimized"))
    #:stop (make-kill-destructor)))
