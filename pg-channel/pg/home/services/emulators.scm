(define-module (pg home services emulators)
  #:use-module (gnu home services)
  #:use-module (gnu packages emulators)
  #:use-module (gnu packages games)
  #:use-module (gnu services)
  #:export (home-emulators-service-type))

(define (home-emulators-profile-service config)
  (list desmume
        dosbox-staging
        mgba
        moonlight-qt
        ;; pcsxr ;; Outputs error at build time
        ;; ppsspp ;; Outputs error
        ))

(define home-emulators-service-type
  (service-type (name 'home-emulators)
                (description "Emulators for gaming")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-emulators-profile-service)))
                (default-value #f)))
