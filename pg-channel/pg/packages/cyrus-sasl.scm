(define-module (pg packages cyrus-sasl)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cyrus-sasl-xoauth2
  (package
    (name "cyrus-sasl-xoauth2")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1py9f1mn5k5xihrk0lfrwr6723c22gjb7lmgya83ibvislm2x3wl"))))
    (build-system gnu-build-system)
    (arguments
     ;; Not sure what is required here in order to get this installed into the
     ;; correct location. Maybe it needs to be installed in the plugindir for
     ;; the cyrus-sasl package above? Not sure how to reference that directory
     ;; in this package though.
    '(#:configure-flags (list (string-append "--prefix="
                                             (assoc-ref %outputs "out")
                                             "/lib/sasl2"))))
    (native-inputs (list autoconf automake libtool))
    (inputs (list cyrus-sasl))
    (home-page "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
    (synopsis "XOAUTH2 mechanism plugin for cyrus-sasl")
    (description "XOAUTH2 mechanism plugin for cyrus-sasl.")
    (license (list license:expat license:expat))))


(define-public cyrus-sasl-xoauth2
  (package
    (name "cyrus-sasl-xoauth2")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/moriyoshi/cyrus-sasl-xoauth2/archive/refs/tags/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0hz95rp2n0xrkc0y2fgif2s7d61hlhd4qki5m8q0s44qc1b2cb56"))))
    (build-system gnu-build-system)
    (synopsis "This is a plugin implementation of XOAUTH2")
    (description "This is a plugin implementation of XOAUTH2")
    (home-page "https://github.com/moriyoshi/cyrus-sasl-xoauth2/tree/v0.2")
    (license license:expat)))
