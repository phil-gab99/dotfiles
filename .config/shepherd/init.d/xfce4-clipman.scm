(define xfce4-clipman
  (service '(xfce4-clipman)
    #:documentation "Runs `xfce4-clipman"
    #:respawn? #t
    #:start (make-forkexec-constructor '("xfce4-clipman"))
    #:stop (make-kill-destructor)))
