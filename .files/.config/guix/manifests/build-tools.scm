(use-modules (gnu)
             (gnu packages)
             (guix profiles))

(use-package-modules base build-tools cmake glib guile mail ninja pkg-config
                     search texinfo)

(packages->manifest
 (list cmake
       (specification->package "clang@17")
       (specification->package "gcc-toolchain@11")
       glib
       gmime
       gnu-make
       guile-3.0
       meson
       ninja
       pkg-config
       texinfo
       xapian))
