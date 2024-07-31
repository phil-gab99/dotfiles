(define-module (pg packages scala)
  #:use-module (gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(use-package-modules base compression gcc java)

(define-public scala
  (package
    (name "scala")
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lampepfl/dotty/releases/download/"
                           version "/scala3-" version ".tar.gz"))
       (sha256
        (base32 "0jrhhy0k995lg0apqh8ldv7fhl2kgv49c21hs2j34rbc2aaz0ir4"))))
    (build-system copy-build-system)
    (propagated-inputs (list openjdk11))
    (arguments
     '(#:install-plan
       '(("VERSION" "share/scala/")
         ("bin" "bin" #:exclude-regexp (".*.bat$"))
         ("lib" "lib"))))
    (home-page "https://scala-lang.org/")
    (synopsis "A programming language that scales with you: from small scripts
to large multiplatform applications.")
    (description "Scala is a modern multi-paradigm programming language designed
to express common programming patterns in a concise, elegant, and type-safe way.
 It seamlessly integrates features of object-oriented and functional
languages.")
    (license license:asl2.0)))

(define-public sbt
  (package
    (name "sbt")
    (version "1.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/" name "/" name
                                  "/releases/download/v" version "/" name "-"
                                  version ".tgz"))
              (sha256
               (base32 "1wsrsqjvfn1yl4848xlyf5zbwk1328y2yrr2kzk4dvlqjk79izj7"))))
    (build-system copy-build-system)
    (native-inputs
     (list glibc zlib (list gcc "lib")))
    (arguments
     '(#:install-plan
       '(("LICENSE" "share/sbt/")
         ("NOTICE" "share/sbt/")
         ("conf" "etc/sbt/")
         ("bin" "bin" #:exclude-regexp (".*.bat$"
                                        ".*apple-darwin$"
                                        ".*win32.exe$")))
       #:validate-runpath? #f))
    (home-page "https://www.scala-sbt.org/index.html")
    (synopsis "The interactive build tool. Define your tasks in Scala. Run them
in parallel from sbt's interactive shell.")
    (description "sbt is built for Scala and Java projects. It is the build tool
of choice for 93.6% of the Scala developers (2019). One of the examples of
Scala-specific feature is the ability to cross build your project against
multiple Scala versions.")
    (license license:asl2.0)))
