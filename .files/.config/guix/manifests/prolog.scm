(use-modules (gnu)
             (guix profiles))

(use-package-modules prolog)

(packages->manifest
 (list gprolog))
