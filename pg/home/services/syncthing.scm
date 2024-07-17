(define-module (pg home services syncthing)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-syncthing-service-type
            home-syncthing-configuration))

(use-package-modules syncthing)
(use-service-modules shepherd)

(define-record-type* <home-syncthing-configuration>
  home-syncthing-configuration
  make-home-syncthing-configuration
  home-syncthing-configuration?
  (syncthing home-syncthing-configuration-syncthing
             (default syncthing))
  (syncthing-gtk home-syncthing-configuration-syncthing-gtk
                 (default syncthing-gtk))
  (user home-syncthing-configuration-user
        (default #f))
  (home home-syncthing-configuration-home
        (default #f)))

(define (home-syncthing-profile-service config)
  (list (home-syncthing-configuration-syncthing-gtk config)))

(define (home-syncthing-shepherd-service config)
  (match-record config <home-syncthing-configuration>
   (syncthing user home)
   (shepherd-service
    (provision '(syncthing))
    (documentation "Runs syncthing")
    (start #~(make-forkexec-constructor
              (list #$(file-append syncthing "/bin/syncthing")
                    "--no-browser"
                    "--no-restart")
              #:user #$user
              #:environment-variables
              (append (list (string-append "HOME="
                                           (or #$home
                                               (passwd:dir (getpw #$user))))
                            "SSL_CERT_DIR=/etc/ssl/certs"
                            "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                      (filter (negate
                               (lambda (str)
                                 (or (string-prefix? "HOME=" str)
                                     (string-prefix? "SSL_CERT_DIR=" str)
                                     (string-prefix? "SSL_CERT_FILE=" str))))
                              (environ)))))
    (stop #~(make-kill-destructor))
    (respawn? #f))))

(define (home-syncthing-gtk-shepherd-service config)
  (match-record config <home-syncthing-configuration>
   (syncthing-gtk user home)
   (shepherd-service
    (provision '(syncthing-gtk))
    (requirement '(syncthing))
    (documentation "Runs syncthing-gtk")
    (start #~(make-forkexec-constructor
              (list #$(file-append syncthing-gtk "/bin/syncthing-gtk")
                    "--minimized")
              #:environment-variables
              (cons "WAYLAND_DISPLAY=wayland-1"
                    (default-environment-variables))))
    (stop #~(make-kill-destructor)))))

(define (home-syncthing-shepherd-services config)
  (list (home-syncthing-shepherd-service config)
        (home-syncthing-gtk-shepherd-service config)))

(define home-syncthing-service-type
  (service-type (name 'home-syncthing)
                (description "Syncthing service")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-syncthing-profile-service)
                       (service-extension home-shepherd-service-type
                                          home-syncthing-shepherd-services)))
                (default-value (home-syncthing-configuration))))
