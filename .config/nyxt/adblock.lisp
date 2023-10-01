(in-package #:nyxt)

(define-configuration nyxt/blocker-mode:blocker-mode
  ((nyxt/blocker-mode:hostlists
    (mapcar (lambda (h)
      	;; Update every minute
      	(setf (nyxt/blocker-mode:update-interval h) 60))
            %slot-default%))))

(define-configuration buffer
  ((default-modes (append '(blocker-mode noscript-mode) %slot-default%))))
