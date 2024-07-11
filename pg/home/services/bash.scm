(define-module (pg home services bash)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-bash-service-type
            home-bash-configuration))

(use-package-modules bash)
(use-service-modules configuration)

(define-configuration home-bash-configuration
  (bash-profile
   (text-config '())
   "File template for .bash_profile")
  (bashrc
   (text-config '())
   "File template .bash_profile"))

(define (home-bash-profile-service config)
  (list bash))

(define (home-bash-files-service config)
  (define (filter-fields field)
    (filter-configuration-fields home-bash-configuration-fields
                                 (list field)))
  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  `((".bash_profile"
     ,(mixed-text-file "bash_profile"
                       "\
# Set up the system, user profile, and related variables.
# /etc/profile will be sourced by bash automatically
# Set up the home environment profile.
[ -f ~/.profile ] && . ~/.profile

# Honor per-interactive-shell startup file
[ -f ~/.bashrc ] && . ~/.bashrc"
                       (serialize-field 'bash-profile)))
    (".bashrc"
     ,(mixed-text-file "bashrc"
                       (serialize-field 'bashrc)
                       "\
# Source the system-wide file.
[ -f /etc/bashrc ] && . /etc/bashrc"))))

(define home-bash-service-type
  (service-type (name 'home-bash)
                (description "Service for configuring bash environment")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-bash-profile-service)
                       (service-extension home-files-service-type
                                          home-bash-files-service)))
                (default-value (home-bash-configuration))))
