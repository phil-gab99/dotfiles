(define-module (pg home services emulators)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (home-emulators-service-type))

(use-package-modules emulators)

(define (home-emulators-profile-service config)
  (list desmume
        dosbox-staging
        ;; ppsspp ;; Outputs error
        ))

(define home-emulators-service-type
  (service-type (name 'home-emulators)
                (description "Emulators for gaming")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-emulators-profile-service)))
                (default-value #f)))
