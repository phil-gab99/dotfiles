(define-module (pg home services gammastep)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-gammastep-service-type))

(use-package-modules xdisorg)
(use-service-modules shepherd)

(define (home-gammastep-shepherd-service config)
  (shepherd-service
   (provision '(gammastep))
   (documentation "Run `gammastep-indicator'")
   (auto-start? #f)
   (start #~(make-forkexec-constructor
             (list #$(file-append gammastep "/bin/gammastep-indicator"))
             #:environment-variables
             (cons "WAYLAND_DISPLAY=wayland-1"
                   (default-environment-variables))))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-gammastep-shepherd-services config)
  (list (home-gammastep-shepherd-service config)))

(define home-gammastep-service-type
  (service-type (name 'home-gammastep)
                (description "Service for running `gammastep'")
                (extensions
                 (list (service-extension home-shepherd-service-type
                                          home-gammastep-shepherd-services)))
                (default-value #f)))
