(use-modules
 (shepherd support))

(define syncthing
  (service '(syncthing)
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing" "--no-browser"))
    #:stop (make-kill-destructor)))

(register-services (list syncthing))
(start-in-the-background '(syncthing))

(use-modules
 (shepherd support))

(define syncthing-gtk
  (service '(syncthing-gtk)
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing-gtk" "--minimized"))
    #:stop (make-kill-destructor)))

(register-services (list syncthing-gtk))
