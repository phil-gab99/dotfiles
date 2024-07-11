(define-module (pg home services udiskie)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-udiskie-service-type))

(use-package-modules freedesktop)
(use-service-modules shepherd)

(define (home-udiskie-shepherd-service config)
  (shepherd-service
   (provision '(udiskie))
   (documentation "Run `udiskie'")
   (start #~(make-forkexec-constructor
             (list #$(file-append udiskie "/bin/udiskie") "-t")))
   (stop #~(make-kill-destructor))))

(define (home-udiskie-shepherd-services config)
  (list (home-udiskie-shepherd-service config)))

(define home-udiskie-service-type
  (service-type (name 'home-udiskie)
                (description "Service for running `udiskie'")
                (extensions
                 (list (service-extension home-shepherd-service-type
                                          home-udiskie-shepherd-services)))
                (default-value #f)))
