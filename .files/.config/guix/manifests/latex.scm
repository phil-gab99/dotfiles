(use-modules (gnu)
             (guix profiles))

(use-package-modules tex texlive)

(packages->manifest
 (list rubber
       texlive))
