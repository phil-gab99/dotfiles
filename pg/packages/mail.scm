(define-module (pg packages mail)
  #:use-module (gnu packages mail)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define-public mu-stable
  (package
    (inherit mu)
    (version "1.10.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/djcb/mu/releases/download/v"
                           version "/mu-" version ".tar.xz"))
       (sha256
        (base32 "129m6rz8vbd7370c3h3ma66bxqdkm6wsdix5qkmv1vm7sanxh4bb"))))
    (inputs
     (modify-inputs (package-inputs mu)
                    (delete "readline" "python")))
    (arguments
     (list
      #:modules '((guix build meson-build-system)
                  (guix build emacs-utils)
                  (guix build utils))
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build emacs-utils))
      #:configure-flags
      #~(list (format #f "-Dguile-extension-dir=~a/lib" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-bin-references
            (lambda _
              (substitute* '("guile/tests/test-mu-guile.cc"
                             "mu/tests/test-mu-cmd.cc"
                             "mu/tests/test-mu-cmd-cfind.cc"
                             "mu/tests/test-mu-query.cc")
                (("/bin/sh") (which "sh")))
              (substitute* '("lib/tests/bench-indexer.cc"
                             "lib/utils/mu-test-utils.cc")
                (("/bin/rm") (which "rm")))))
          (add-after 'install 'fix-ffi
            (lambda _
              (substitute* (find-files #$output "mu.scm")
                (("\"libguile-mu\"")
                 (format #f "\"~a/lib/libguile-mu\"" #$output)))))
          (add-after 'install 'install-emacs-autoloads
            (lambda* (#:key outputs #:allow-other-keys)
              (emacs-generate-autoloads
               "mu4e"
               (string-append (assoc-ref outputs "out")
                              "/share/emacs/site-lisp/mu4e")))))))))
