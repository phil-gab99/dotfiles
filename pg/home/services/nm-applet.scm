(define-module (pg home services nm-applet)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-nm-applet-service-type))

(use-package-modules gnome)
(use-service-modules shepherd)

(define (home-nm-applet-profile-service config)
  (list network-manager-applet))

(define (home-nm-applet-shepherd-service config)
  (shepherd-service
   (provision '(nm-applet))
   (documentation "Run `nm-applet'")
   (auto-start? #f)
   (start #~(make-forkexec-constructor
             (list #$(file-append network-manager-applet "/bin/nm-applet"))
             #:environment-variables
             (cons "WAYLAND_DISPLAY=wayland-1"
                   (default-environment-variables))))
   (stop #~(make-kill-destructor))))

(define (home-nm-applet-shepherd-services config)
  (list (home-nm-applet-shepherd-service config)))

(define home-nm-applet-service-type
  (service-type (name 'home-nm-applet)
                (description "Service for running `nm-applet'")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-nm-applet-profile-service)
                       (service-extension home-shepherd-service-type
                                          home-nm-applet-shepherd-services)))
                (default-value #f)))
