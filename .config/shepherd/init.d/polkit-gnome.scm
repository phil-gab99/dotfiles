(define polkit-gnome
  (make <service>
    #:provides '(polkit-gnome)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/phil-gab99/.guix-extra-profiles/desktop/desktop/libexec/polkit-gnome-authentication-agent-1"))
    #:stop (make-kill-destructor)))

(register-services polkit-gnome)
(start polkit-gnome)
