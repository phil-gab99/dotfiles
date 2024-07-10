(define polkit-gnome
  (service '(polkit-gnome)
    #:documentation "Runs `polkit-gnome'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/phil-gab99/.guix-extra-profiles/desktop/desktop/libexec/polkit-gnome-authentication-agent-1"))
    #:stop (make-kill-destructor)))
