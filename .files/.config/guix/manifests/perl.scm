(use-modules (gnu)
             (guix profiles))

(use-package-modules perl)

(packages->manifest
 (list perl))
