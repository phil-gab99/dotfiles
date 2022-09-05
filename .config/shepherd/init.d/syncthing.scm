(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing" "-no-browser"))
    #:stop (make-kill-destructor)))

(register-services syncthing)
(start syncthing)

(define syncthing-gtk
  (make <service>
    #:provides '(syncthing-gtk)
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing-gtk" "--minimized"))
    #:stop (make-kill-destructor)))

(register-services syncthing-gtk)
