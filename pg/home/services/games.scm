(define-module (pg home services games)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (home-games-service-type))

(use-package-modules emulators)

;; Retrieve save files under ~/.var/app/org.desmume.DeSmuME/config/desmume/
;; Retrieve save files under ~/.var/app/org.ppsspp.PPSSPP/config/ppsspp/
(define (home-games-profile-service config)
  (list desmume
        dosbox-staging
        ppsspp))

(define home-games-service-type
  (service-type (name 'home-games)
                (description "Emualators for gaming")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-games-profile-service)))
                (default-value #f)))
