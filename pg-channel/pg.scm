(define-module (pg)
  #:export (use-home-service-modules
            use-pg-home-service-modules
            use-pg-system-modules))

(define-syntax-rule (use-home-service-modules module ...)
  (use-modules (gnu home services module) ...))

(define-syntax-rule (use-pg-home-service-modules module ...)
  (use-modules (pg home services module) ...))

(define-syntax-rule (use-pg-system-modules module ...)
  (use-modules (pg system module) ...))
