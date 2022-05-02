(in-package #:nyxt-user)

;;;; This is a file with settings for my nx-search-engines extension.

;;; My DDG settings, shared between the usual, image-search and other
;;; types of DuckDuckGo.
(defvar *duckduckgo-keywords*
  '(:theme :dark
    :help-improve-duckduckgo nil
    :homepage-privacy-tips nil
    :privacy-newsletter nil
    :newsletter-reminders nil
    :install-reminders nil
    :install-duckduckgo nil
    :units-of-measure :metric
    :keyboard-shortcuts t
    :advertisements nil
    :open-in-new-tab nil
    :infinite-scroll t
    :safe-search :off
    :font-size :medium
    :header-behavior :on-fixed
    :font :helvetica
    :background-color "000000"
    :center-alignment t))

(define-configuration buffer
    ((search-engines
      (list
       ;; engines: is a prefix for `nx-search-engines',
       ;; it only works if you load nx-search-engines.
       (engines:google :shortcut "gmaps"
		       :object :maps)
       (engines:wikipedia :shortcut "w")
       (engines:google :shortcut "g"
		       :safe-search nil)
       (apply #'engines:duckduckgo-images
	      :shortcut "i" *duckduckgo-keywords*)
       (engines:duckduckgo-html-only :shortcut "dho")
       (engines:github :shortcut "git")
       (apply #'engines:duckduckgo
	      :shortcut "d" *duckduckgo-keywords*)))))
