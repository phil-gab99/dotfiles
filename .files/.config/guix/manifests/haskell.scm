(use-modules (gnu)
             (guix profiles))

(use-package-modules haskell)

(packages->manifest
 (list ghc
       (list ghc "doc")))
