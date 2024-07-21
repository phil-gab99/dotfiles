(define-module (pg home services bash)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 textual-ports)
  #:export (home-bash-service-type
            home-bash-configuration))

(define-record-type* <home-bash-configuration>
  home-bash-configuration
  make-home-bash-configuration
  home-bash-configuration?
  (bash-profile home-bash-configuration-bash-profile
                (default #f))
  (bashrc home-bash-configuration-bashrc
          (default #f))
  (bash-logout home-bash-configuration-bash-logout
               (default #f)))

(define (home-bash-profile-service config)
  (list bash))

(define (home-bash-files-service config)
  `((".bash_profile"
     ,(mixed-text-file "bash_profile"
                       "\
# Set up the system, user profile, and related variables.
# /etc/profile will be sourced by bash automatically
# Set up the home environment profile.
[ -f ~/.profile ] && . ~/.profile
# Honor per-interactive-shell startup file
[ -f ~/.bashrc ] && . ~/.bashrc 
"
                       (if (home-bash-configuration-bash-profile config)
                           (call-with-input-file
                               (home-bash-configuration-bash-profile config)
                             get-string-all))))
    (".bashrc"
     ,(plain-file "bashrc"
                  (if (home-bash-configuration-bashrc config)
                      (call-with-input-file
                          (home-bash-configuration-bashrc config)
                        get-string-all)
                      "")))
    (".bash_logout"
     ,(plain-file "bash-logout"
                  (if (home-bash-configuration-bash-logout config)
                      (call-with-input-file
                          (home-bash-configuration-bash-logout config)
                        get-string-all)
                      "")))))

(define home-bash-service-type
  (service-type (name 'home-bash)
                (description "Service for configuring bash environment")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-bash-profile-service)
                       (service-extension home-files-service-type
                                          home-bash-files-service)))
                (default-value (home-bash-configuration))))
