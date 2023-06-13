(use-modules
 (shepherd support))

(define polkit-gnome
  (service '(polkit-gnome)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/phil-gab99/.guix-extra-profiles/desktop/desktop/libexec/polkit-gnome-authentication-agent-1"))
    #:stop (make-kill-destructor)))

(register-services (list polkit-gnome))
(start-in-the-background '(polkit-gnome))
