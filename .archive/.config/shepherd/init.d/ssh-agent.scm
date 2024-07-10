(use-modules
 (shepherd support))

(define ssh-agent
  (service '(ssh-agent)
    #:documentation "Run `ssh-agent'"
    #:respawn? #t
    #:start (lambda ()
              (let ((socket-dir (string-append %user-runtime-dir "/ssh-agent")))
                (unless (file-exists? socket-dir)
                  (mkdir-p socket-dir)
                  (chmod socket-dir #o700))
                (fork+exec-command
                 `("ssh-agent" "-D" "-a" ,(string-append socket-dir "/socket"))
                 #:log-file (string-append %user-log-dir "/ssh-agent.log"))))
    #:stop (make-kill-destructor)))
