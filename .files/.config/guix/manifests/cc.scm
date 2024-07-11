(use-modules (gnu)
             (gnu packages)
             (guix profiles))

(use-package-modules base cpp man)

(packages->manifest
 (list (specification->package "gcc-toolchain@11.3.0")
       man-pages
       glibc
       ccls))
